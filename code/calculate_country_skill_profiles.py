
import pandas as pds
import os
import numpy as np
import platform

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


os.chdir('/Users/markhuberty/Documents/Research/Papers/stackexchange/')


users_skills = pds.read_csv('./data/user_skill_map.csv')
users_geo = pds.read_csv('./data/users_geocoded_final.csv')

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


