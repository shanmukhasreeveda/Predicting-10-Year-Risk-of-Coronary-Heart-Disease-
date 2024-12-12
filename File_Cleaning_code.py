import pandas as pd

# Replace 'your_file.csv' with the actual path to your CSV file
file_path = '/Users/sreevedatippavajhala/Library/CloudStorage/OneDrive-UniversityoftheFraserValley/Documents/Winter/STAT 315/project/train.csv'

# Read the CSV file into a pandas DataFrame
df = pd.read_csv(file_path)

# Remove rows with all NaN values (blank rows)
df_cleaned = df.dropna(how='any')

# Write the cleaned DataFrame back to a CSV file
df_cleaned.to_csv('cleaned_file.csv', index=False,header= False)

print('Blank rows removed and cleaned file saved as cleaned_file.csv')
