
import pandas as pds
import os
import numpy as np
import platform
import scipy
import scipy.sparse as sp

def calculate_rxa(input_df):
    """
    Calculates revealed () advantage, where () can be comparative (trade),
    technological (patents), etc.
    Takes as input a sparse DataFrame form (geo * class), where cells
    are aggregate values (e.g., country * trade_code, export_volume).
    Computes RXA as
    (geo share of value in class) / (world share of value in class).
    Returns a sparse DataFrame of form geo:class:rxa
    """
    ## Get the denominator
    ## produces a class * 1 df
    world_total_volume = input_df.sum().sum()
    world_product_volume = input_df.sum(axis=0).astype(float)
    world_product_share = world_product_volume / world_total_volume
    ## Get the numerator
    ## produces a class * geo df
    country_total_volume = input_df.sum(axis=1).astype(float)
    country_product_share = input_df.transpose() / (country_total_volume)
    ## Generate the RXAs
    ## Want to match row indices, hence transpose. Take advantage
    ## of python's auto-align on index.
    rxa = country_product_share.transpose() / world_product_share
    return rxa

os.chdir('/home/markhuberty/Documents/stackexchange')
#os.chdir('/Users/markhuberty/Documents/Research/Papers/stackexchange/')


users_skills = pds.read_csv('./data/user_skill_map.csv')
users_geo = pds.read_csv('./data/users_geocoded_final.csv', index_col=1)
users_skills_wide = users_skills.pivot('userid', 'skillid', 'skill_value').to_sparse()

users_rxa = calculate_rxa(users_skills_wide)
user_specialization_count = (users_rxa > 1).sum(axis=1)
user_specialization_count.name = 'usr_spec_ct'
test = users_geo.join(user_specialization_count, how='inner')

test_group = test.groupby('country.code')
ctry_spec = test_group.agg({'usr_spec_ct':np.median})

test.to_csv('./data/ctry_spec_test.csv')

skills_geo = pds.merge(users_skills,
                       users_geo,
                       how='inner',
                       left_on='userid',
                       right_on='Id'
                       )

## Write out the mergefile
skills_geo[['userid',
            'skillid',
            'skill_value',
            'country.code',
            'Reputation',
            'CreationDate',
            'LastAccessDate']].to_csv('./data/users_skills_geo.csv')

## Take some generic RXA and diversification data
## Note that this does nothing to control for duration/reputation confounders

skills_geo_new = skills_geo[['userid', 'skillid', 'skill_value', 'country.code']]
skills_geo_group = skills_geo_new.groupby(['country.code','skillid'])
country_mean_skill_levels = skills_geo_group.aggregate(np.mean)['skill_value']
country_skill_levels_wide = country_mean_skill_levels.unstack()

## This is fine, but doesn't handle the problem of duration, etc. 
skill_rxa = calculate_rxa(country_skill_levels_wide)
country_skill_diversification = (skill_rxa > 1).sum(axis=1)
country_skill_diversification.sort()

# user_skill; right now, super-slow. Would be faster
# w/ sparse matrix...
users_skills_group = users_skills.groupby(['userid','skillid'])
users_skills_wide = users_skills_group.unstack()




skillids = list(set(users_skills['skillid']))
userids = list(set(users_skills['userid']))

idx_x = []
idx_y = []
val = []

test = {}
userids_long = users_skills['userid']
for i, r in enumerate(userids_long):
    if test.has_key(r):
        idx_x.append(test[r])
    else:
        test[r] = userids.index(r)
        idx_x.append(test[r])

users_skills_csc = sp.csc_matrix((array(users_skills['skill_value']),
                                  (idx_x, list(users_skills['skillid']))),
                                 shape=(len(userids), len(skillids))
                                 )
users_skills_mat = users_skills_csc.todense()

total_skill_score = users_skills_mat.sum()
skill_scores = users_skills_mat.sum(axis=0).tolist()
skill_scores = np.array(skill_scores)
global_skill_share = skill_scores / total_skill_score

user_skill_share = users_skills_mat.copy()

for i in range(user_skill_share.shape[0]):
    vec = user_skill_share[i, ]
    vec = np.array([vec[0,j] for j in range(vec.shape[0])])
    vec_total = sum(vec)
    if vec_total < 1 and vec_total > -1:
        vec_total = round(vec_total)
    vec = vec / vec_total
    test = [v for v in vec if v > 1 or v < -1]
    if len(test) > 0:
        print i
    user_skill_share[i, ] = vec

rsa = user_skill_share.copy()
for j, v in enumerate(global_skill_share):
    rsa[:,j] = user_skill_share[:,j] / v

blah = [sum(rsa[i,] > 1) for i in range(rsa.shape[0])]
    
for i,d in enumerate(user_skill_share.data):
    if np.isnan(d) or np.isinf(d):
        user_skill_share.data[i] = 0

rsa = users_skills_csc.copy().tocsr()
for i,v in enumerate(global_skill_share):
    idx_start = users_skills_csc.indptr[i]
    idx_end = users_skills_csc.indptr[i+1]
    #idxs = users_skills_csc.indices[idx_start:idx_end]
    rsa.data[idx_start:idx_end] = user_skill_share[idx_start:idx_end] / v

rsa.data[np.isnan(rsa.data)] = 0

## Here's not right. Needs the right form of division to generate the
## proper csc matrix form. See compute_intl_hs6_pathlengths for form of the
## code.
