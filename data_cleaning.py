import pandas as pd

# Define chunk size
chunk_size = 100000  # Adjust this depending on your system's memory

# Use an iterator to load chunks
business_iterator = pd.read_json('business.json', lines=True, chunksize=chunk_size)

# Filter businesses in CA and concatenate into one DataFrame
business_data_ca = pd.concat([chunk[chunk['state'] == 'FL'] for chunk in business_iterator])

# Save the California business data to CSV (it should be small enough)
business_data_ca.to_csv('business_data_fl.csv', index=False)

# Now read the reviews in chunks and iteratively merge with business_data_ca
review_iterator = pd.read_json('review.json', lines=True, chunksize=chunk_size)

# We'll append each filtered and merged chunk to a CSV file
first_chunk = True  # To write headers in the first chunk only
for chunk in review_iterator:
    # Merge with the filtered business data
    merged_chunk = pd.merge(business_data_ca, chunk, on='business_id', how='inner')
    
    # Append to a CSV file
    with open('reviews_in_fl.csv', 'a', newline='', encoding='utf-8') as f:
        merged_chunk.to_csv(f, index=False, header=first_chunk)
        first_chunk = False  # Disable header writing for subsequent chunks

# After the loop, 'reviews_in_ca.csv' will have the merged data of reviews for businesses in CA
