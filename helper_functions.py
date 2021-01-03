# -*- coding: utf-8 -*-
"""
Created on Tue Jun 30 17:49:03 2020

@author: Gulati Sahil
"""
import pandas as pd
import numpy as np
######################################################################
# Create utility functions
######################################################################
def default(ay):
    ay_default="AY"+"_".join([str(int(i)-1) for i in ay.replace("AY","").split("_")])
    return ay_default

def percentile(n):
    def percentile_(x):
        return np.percentile(x, n)
    percentile_.__name__ = 'percentile_%s' % n
    return percentile_

def exogenous(df,registration_start,external_date,ex_num=1):
    df["GROUP1"]=df.apply(lambda x: x['GROUP1']+"_EX" if x['TIME']>ex_days else x['GROUP1'],axis=1)
    df["ex"]=df.apply(lambda x: 1 if x['TIME']>ex_days else 0,axis=1)
    df["TIME"]=df.apply(lambda x: x['TIME']-ex_days if x['TIME']>ex_days else x['TIME'],axis=1)
    return df

def bestfit(df,bestfit):
    import pandas as pd
    print('bestfit1')
    findbest=df.groupby('GROUP1')['TIME'].max().reset_index()
    print('bestfit2')
    findbest=pd.merge(findbest,bestfit,on='GROUP1',how='left')
    print('bestfit3')
    findbest['closest']=abs(findbest['TIME']-findbest['DAYS_PASSED'])
    findbest=pd.merge(findbest,findbest.groupby(['GROUP1'])['closest'].min().reset_index(),on='GROUP1',how='left')
    findbest=findbest[(findbest['closest_x']==findbest['closest_y']) | (findbest['closest_x'].isna() & findbest['closest_y'].isna())][['GROUP1','MODEL_ID','BESTFIT','BESTFIT_ERROR','OVERALL_ENROLLMENT','DEFAULT_ERROR']].drop_duplicates(['GROUP1'])
    return findbest


def bestfit_diagnosis(df,bestfit):
    import pandas as pd
    print('bestfit1')
    findbest=df.groupby('GROUP1')['TIME'].max().reset_index()
    print('bestfit2')
    findbest=pd.merge(findbest,bestfit,on='GROUP1',how='left')
    print('bestfit3')
    findbest['closest']=abs(findbest['TIME']-findbest['DAYS_PASSED'])
    findbest=pd.merge(findbest,findbest.groupby(['GROUP1'])['closest'].min().reset_index(),on='GROUP1',how='left')
    findbest=findbest[(findbest['closest_x']==findbest['closest_y']) | (findbest['closest_x'].isna() & findbest['closest_y'].isna())][['GROUP1','MODEL_ID','BESTFIT','BESTFIT_ERROR','OVERALL_ENROLLMENT']].drop_duplicates(['GROUP1'])
    return findbest
