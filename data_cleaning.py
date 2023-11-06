import pandas as pd
import json

chunk_size = 10000 

business_chunks = pd.read_json('business.json', lines=True, chunksize=chunk_size)

business_data_ca_list = []

for chunk in business_chunks:
    business_data_ca_chunk = chunk[chunk['state'] == 'CA']
    business_data_ca_list.append(business_data_ca_chunk)

business_data_ca = pd.concat(business_data_ca_list, ignore_index=True)


review_chunks = pd.read_json('review.json', lines=True, chunksize=chunk_size)

merged_data_list = []

for chunk in review_chunks:
    merged_chunk = pd.merge(business_data_ca, chunk, on='business_id', how='inner')
    merged_data_list.append(merged_chunk)

merged_data = pd.concat(merged_data_list, ignore_index=True)

