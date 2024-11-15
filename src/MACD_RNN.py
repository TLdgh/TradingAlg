import pandas as pd
import numpy as np
import os
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import LSTM, Dense, Dropout
from tensorflow.keras.preprocessing.sequence import pad_sequences
from sklearn.metrics import accuracy_score, classification_report, confusion_matrix

def extract_windows(data):
    groups = data['group'].values
    unique_groups = np.unique(groups)

    windows = []
    window_labels = []

    for i in range(len(unique_groups) - 1):
        pairs = unique_groups[i:i + 2]  # Select pairs of adjacent groups (start and end index)
        
        # Extract the data for the two consecutive groups
        subdata = data[data['group'].isin(pairs)]
        
        # Create the window of MACD values
        window = subdata['MACD_Histogram'].values
        windows.append(window)  # Add the MACD histogram data to the window list
        
        # Check for divergence condition
        # If the minimum of the second group is greater than the first, it's a divergence (label 1)
        group1_min = subdata.loc[subdata['group'] == pairs[0], 'MACD_Histogram'].min()
        group2_min = subdata.loc[subdata['group'] == pairs[1], 'MACD_Histogram'].min()
        
        # Label the window as 1 for divergence, 0 for no divergence
        if group2_min > group1_min:
            window_labels.append(1)
        else:
            window_labels.append(0)

    # Convert to numpy arrays for model training
    window_labels = np.array(window_labels)
    
    # Pad the windows to have a consistent length (if needed)
    windows = pad_sequences(windows, padding='post', dtype='float32')

    # Reshape the data to match the LSTM input shape (samples, time steps, features)
    # If each "window" is a sequence of MACD histogram values, each sequence has 1 feature (MACD_Histogram)
    windows = windows.reshape(windows.shape[0], windows.shape[1], 1)  # Shape: (samples, time steps, features)
    
    return {"windows": windows, "window_labels": window_labels}

if __name__ == "__main__":
    currentfile_path = os.path.abspath(__file__)
    root_directory = os.getcwd()
    data_path = root_directory + '/Data/NQ/your_data.csv'
    
    # Step 1: Load the fit data
    fitdata = pd.read_csv(data_path)
    
    # Extract windows from the fitdata
    modeldata = extract_windows(fitdata)




    # Step 2: Define the LSTM Model
    model = Sequential()
    model.add(LSTM(units=50, return_sequences=False, input_shape=(modeldata['windows'].shape[1], 1)))  # (time steps, features)
    model.add(Dropout(0.2))  # Dropout to prevent overfitting
    model.add(Dense(1, activation='sigmoid'))  # Output layer with sigmoid activation (binary classification)
    
    # Compile the model
    model.compile(optimizer='adam', loss='binary_crossentropy', metrics=['accuracy'])

    # Print the model summary
    model.summary()

    # Step 3: Train the Model (no validation_split)
    history = model.fit(modeldata['windows'], modeldata['window_labels'], epochs=20, batch_size=32, validation_split=0.2)
    
    # Step 4: Evaluate the model on the validation set
    val_windows = modeldata['windows'][-int(len(modeldata['windows']) * 0.2):]  # Last 20% of windows
    val_labels = modeldata['window_labels'][-int(len(modeldata['window_labels']) * 0.2):]  # Last 20% of labels
    
    # Make predictions on the validation set
    predictions = model.predict(val_windows)  # This will return probabilities
    
    # Convert probabilities to binary class labels (0 or 1)
    predicted_labels = (predictions.flatten() > 0.5).astype(int)  # Applying 0.5 threshold for binary classification

    # Print predictions and labels
    print("Predictions (probabilities):")
    print(predictions)
    print("Predicted labels (0 or 1):")
    print(predicted_labels)
    
    # Calculate accuracy on the validation set
    accuracy = np.mean(predicted_labels == val_labels)
    print(f"Validation accuracy: {accuracy}")
    
    
    
    
    # Step 5: Prediction
    testdata = pd.read_csv(os.getcwd() + '/Data/NQ/QQQ_data.csv')
    preddata = extract_windows(testdata)

    # Make predictions on the test data
    predictions = model.predict(preddata['windows'])
    
    # Convert probabilities to binary class labels (0 or 1)
    predicted_labels = (predictions.flatten() > 0.5).astype(int)

    # Print the predictions and predicted labels
    print("Predictions (probabilities):")
    print(predictions)
    print("Predicted labels (0 or 1):")
    print(predicted_labels)
    
    # Calculate accuracy on the validation set
    testdata=testdata.drop_duplicates(subset='group', keep='last')
    testdata['bullish_divergence']=np.append(0, preddata['window_labels'])
    testdata['val_labels']= np.append(0, predicted_labels)
    accuracy = np.mean(testdata['bullish_divergence'] == testdata['val_labels'])
    print(f"Validation accuracy: {accuracy}")

    x=testdata[testdata['bullish_divergence']==0]
    print(x.shape)
    x[x['val_labels']==1].shape
    
    
    
    # Evaluate accuracy using sklearn
    accuracy = accuracy_score(preddata['window_labels'], predicted_labels)
    print(f"Accuracy: {accuracy:.4f}")

    # More detailed classification report
    print(classification_report(preddata['window_labels'], predicted_labels))
    
    # Confusion matrix
    conf_matrix = confusion_matrix(preddata['window_labels'], predicted_labels)
    print("Confusion Matrix:\n", conf_matrix)