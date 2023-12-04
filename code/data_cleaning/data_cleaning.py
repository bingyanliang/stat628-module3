import pandas as pd

chunk_size = 100000 

business_iterator = pd.read_json('business.json', lines=True, chunksize=chunk_size)

business_data_ca = pd.concat([chunk[chunk['state'] == 'FL'] for chunk in business_iterator])

business_data_ca.to_csv('business_data_fl.csv', index=False)

review_iterator = pd.read_json('review.json', lines=True, chunksize=chunk_size)

first_chunk = True 
for chunk in review_iterator:

    merged_chunk = pd.merge(business_data_ca, chunk, on='business_id', how='inner')
    
    with open('reviews_in_fl.csv', 'a', newline='', encoding='utf-8') as f:
        merged_chunk.to_csv(f, index=False, header=first_chunk)
        first_chunk = False

