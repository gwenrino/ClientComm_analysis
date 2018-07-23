# collection of functions to use for analysis
import pandas as pd
import numpy as np
import pickle
from sklearn.utils import resample
from sklearn.feature_extraction.text import CountVectorizer
import matplotlib.pyplot as plt

import datefinder as datefi
import regex as re

from textblob import TextBlob

# ***** some common constants ************

fname_messages = 'messages.csv'
fname_surveys = 'surveys.csv'
fname_outbound_topic_model = 'lda_outbound_topic_model.sav'
fname_inbound_topic_model = 'lda_inbound_topic_model.sav'

n_samples = 2000 # parameters for topic modeling
n_features = 1000
n_components = 15
n_top_words = 10

date_pattern = r'\b(?i)(?<=date|court|report|on|for)\s*(\S+\s*\S+\s*\S+\s*\S+\s*\S+)'
address_pattern = r'(?i)(?<=\blocated|\bat[\s*])([0-9]{1,}.*?MD)(\s[0-9]{5})?'
phone_pattern = r'(\+\d{1,2}\s)?\(?\d{3}\)?[\s.-]?\d{3}[\s.-]?\d{4}'

# ***** functions to load data and apply timezone ************

def apply_tz(df,col_name,tz_read='UTC',tz_target='US/Eastern'):
    """ Function to convert to a target timezone. """
    df[col_name] = df[col_name].dt.tz_localize(tz_read).dt.tz_convert(tz_target)
    return df

def make_hour_day(df,col_name):
    """ Function to extract weekday and hour from a time field. """
    df['weekday'] = df[col_name].dt.weekday
    df['weekday_name'] = df[col_name].dt.weekday_name
    df['hour'] = df[col_name].dt.hour
    return df

def rows_missing(df1,df2,col_name):
    """ Function to find rows that are not in two data frames.
        Uses the pd.merge(...,indicator=True) option. """
    df_out = pd.merge(df1,df2,on=col_name,how='left',indicator=True).query('_merge == "left_only"')
    return df_out

def load_messages(fname):
    messages = pd.read_csv(fname, parse_dates=['created_at','send_at','client_created_at'])
    messages = apply_tz(messages,'created_at',tz_read='Etc/GMT-7',tz_target='US/Eastern')
    messages = apply_tz(messages,'send_at',tz_read='Etc/GMT-7',tz_target='US/Eastern')
    messages = apply_tz(messages,'client_created_at',tz_read='Etc/GMT-7',tz_target='US/Eastern')
    return messages

def load_surveys(fname):
    surveys = pd.read_csv(fname, parse_dates=['survey_created_at'])
    surveys = apply_tz(surveys,'survey_created_at',tz_read='Etc/GMT-7',tz_target='US/Eastern')
    return surveys

def load_surveys_single(fname):
    """ Loads survey data with single responses. """
    surveys = load_surveys(fname)
    surveys['survey_created_at'] = surveys['survey_created_at'].apply(lambda x: x.round('s'))
    surveys_single = (surveys
                    .groupby(['client_id','user_id'])[['survey_created_at']]
                    .nunique()
                    .rename(columns={'survey_created_at':'ncounts'})
                    .query('ncounts == 1')
                   .reset_index())
    return pd.merge(surveys_single.drop('ncounts',axis=1),surveys,on=['client_id','user_id'],how='left')

def map_surveys_to_sf(surveys_in):
    surveys = surveys_in.copy()
    surveys['supervision_failure'] = (surveys['survey_response_id'].map({1:False,2:True,3:True,4:True}))
    return surveys


# ***** functions for basic processing ************

def calc_number_messages_exchanged(messages):
    """ Gives the number of messages sent and received. """
    msg_outbound = (messages.query('inbound == False')
                    .groupby(['client_id','user_id'])[['id']]
                    .count())
    msg_inbound = (messages.query('inbound == True')
                   .groupby(['client_id','user_id'])[['id']]
                   .count())
    
    msg_exchanged = pd.concat([msg_outbound,msg_inbound],axis=1).fillna(0)
    msg_exchanged.columns = ['counts_outbound','counts_inbound']
    msg_exchanged['inout_ratio'] = msg_exchanged['counts_inbound'] / msg_exchanged['counts_outbound']
    
    # handle cases with outbound = 0
    msg_exchanged[np.isinf(msg_exchanged['inout_ratio'])] = 0.0
    
    return msg_exchanged

def find_dates_in_string(string,pattern,tz_info,show_result=False):
    """ Extract possible dates from a text string.
    Also, must add time zone information from corresponding sent message."""

    # make sure input is string
    if not isinstance(string,str):
        return []
    
    string_matches = re.findall(pattern, string)
    dates_ext = []
    for match in string_matches:
        if show_result: print(match)
        date_matches = datefi.find_dates(match)
        # since the returned date is of type datetime.datetime,
        # need to convert to Timestamp format for comparison
        # also limit years to UNIX time
        for date_match in date_matches:
            if (date_match.year > 1970) & (date_match.year < 2038):
                if show_result: print(date_match, date_match.timestamp())
                dates_ext.append(pd.Timestamp(date_match.timestamp(),unit='s',tz=tz_info))
    return dates_ext

def find_dates_in_row(row):
    row['dates_ext'] = find_dates_in_string(row['body'],date_pattern,row['send_at'].tzinfo)
    return row

def compare_dates(string,string_target):
    """ Compare dates, drop times less than the target.
    Also, drop times that are 30 days into the future. """
    sext = []
    for s in string:
        if (s >= string_target) & (s <= string_target+pd.Timedelta(days=30)):
            sext.append(s)
    return sext

def compare_dates_in_row(row):
    return [compare_dates(row['dates_ext'],row['send_at'])]

def find_addresses_in_string(string,pattern):
    addresses = []

    # make sure input is string
    if not isinstance(string,str):
        return addresses
    
    string_matches = re.findall(pattern,string)
    for match in string_matches:
        addresses.append(match[0]+match[1])

    return addresses

def find_addresses_in_row(row):
    return [find_addresses_in_string(row['body'],address_pattern)]

def map_word_exist(string,word_str):
    """ Indicate if the word exists. """
    pattern = '(?i)'+word_str
    pattern_raw = r'%s'%pattern
    return 1 if len(find_pattern_in_string(string,pattern_raw)) else 0

def find_pattern_in_string(string,pattern):
    str_matches = []
    
    # make sure input is string
    if not isinstance(string,str):
        return str_matches
    
    matches = re.findall(pattern,string)
    for match in matches:
        str_matches.append(match)
        
    return str_matches

def find_pattern_in_row(row,pattern):
    return [find_pattern_in_string(row['body'],pattern)]

def find_phones_in_row(row):
    return [find_pattern_in_string(row['body'],phone_pattern)]

def convert_to_hr(dfin,col_name,col_target_name):
    """ Converts timestamp into hours. Used for plotting and modeling. """
    df = dfin.copy()
    df[col_target_name] = (df[df[col_name].notnull()][col_name]
                             /pd.Timedelta(hours=1))
    return df

def convert_to_log(dfin,col_name,col_target_name):
    """ Take the natural logarithm of the input column. """
    df = dfin.copy()
    df[col_target_name] = df[col_name].apply(lambda x: np.log(x + 1.e-4))
    return df

def calc_messages_counts(messages,col_name='send_at'):
    """ Calculates message counts per relationship per weekday and hour of day.
    Depends on the time input column. """
    messages_time = messages[['id','client_id','user_id','send_at','inbound']]
    messages_time = make_hour_day(messages_time.copy(),col_name)
    messages_inbound_counts = (messages_time.query('inbound == True')
                               .drop('inbound',axis=1)
                               .groupby(['client_id','user_id','weekday','hour'])[[col_name]]
                              .count()
                              .rename(columns={col_name:'inbound_msg_counts'}))
    
    messages_outbound_counts = (messages_time.query('inbound == False')
                                .drop('inbound',axis=1)
                                .groupby(['client_id','user_id','weekday','hour'])[[col_name]]
                              .count()
                              .rename(columns={col_name:'outbound_msg_counts'}))

    messages_counts = pd.concat([messages_inbound_counts,messages_outbound_counts],axis=1).fillna(0).reset_index()
    
    return messages_counts

def calc_messages_counts_sum(messages):
    messages_counts = calc_messages_counts(messages,col_name='send_at')
    return messages_counts.groupby(['client_id','user_id'])[['inbound_msg_counts','outbound_msg_counts']].sum()

def count_words_per_group(messages,pattern):
    return messages['body'].apply(lambda x: 1 if len(find_pattern_in_string(x,pattern)) else 0).sum()

def count_messages_with_pattern(messages,pattern):
    """ Count messages with a specific pattern. """
    wordcounts = messages.groupby(['client_id','user_id']).apply(lambda x: count_words_per_group(x,word_pattern))
    return pd.DataFrame(wordcounts,columns=['msg_word_counts'],index=wordcounts.index)

def exclude_elem(x,to_exclude):
    return False if (x in to_exclude) else True

def include_elem(x,to_include):
    return True if (x in to_include) else False

def exclude_from_list(x,to_exclude):
    """ Removes elements given a list. """
    return [s for s in filter(lambda y: exclude_elem(y,to_exclude), x)]

# ***** functions dealing with response times ************

def calc_median_response_time_diff_per_group(messages):
    """ Calculates the median time difference between
    sent and received messages. Need to improve on the time calculation. """
    
    # reset index to apply operation within group
    messages = messages.sort_values('send_at',ascending=True).reset_index(drop=True)
    
    # loop over all messages send dates
    tdiff = []
    for i in range(len(messages)):
        #print(i,messages.loc[i,['inbound','send_at']])
        if i == 0:
            date_prev = messages.loc[i,'send_at']
            stat_prev = messages.loc[i,'inbound']
        if stat_prev != messages.loc[i,'inbound']:
            tdiff.append(messages.loc[i,'send_at'] - date_prev)
            date_prev = messages.loc[i,'send_at']
            stat_prev = messages.loc[i,'inbound']

    return [np.median(tdiff),np.amax(tdiff)] if tdiff else [pd.NaT,pd.NaT]

def calc_median_response_time_diff_all(messages):
    """ Calculates the median response time per relationship. """
    median_response_time = messages.groupby(['client_id','user_id']).apply(calc_median_response_time_diff_per_group)
    return pd.DataFrame(median_response_time.values.tolist(),columns=['median_response_time','max_response_time'],index=median_response_time.index)

# The following paired functions can be combined!
def calc_median_response_time_user_to_client(messages):
    """ Calculates the median response times from user to client. """
    messages = messages.sort_values('send_at',ascending=True).reset_index(drop=True)
    
    tdiff = []
    date_prev = pd.NaT
    for i in range(len(messages)):
        if messages.loc[i,'inbound'] == True:
            date_prev = messages.loc[i,'send_at']
        if (messages.loc[i,'inbound'] == False) & (not pd.isna(date_prev)):
            tdiff.append(messages.loc[i,'send_at'] - date_prev)
            date_prev = pd.NaT
            
    return [np.median(tdiff), np.amax(tdiff)] if tdiff else [pd.NaT, pd.NaT]

def calc_median_response_time_client_to_user(messages):
    """ Calculates the median response times from client and user. """
    messages = messages.sort_values('send_at',ascending=True).reset_index(drop=True)
    
    tdiff = []
    date_prev = pd.NaT
    for i in range(len(messages)):
        if messages.loc[i,'inbound'] == False:
            date_prev = messages.loc[i,'send_at']
        if (messages.loc[i,'inbound'] == True) & (not pd.isna(date_prev)):
            tdiff.append(messages.loc[i,'send_at'] - date_prev)
            date_prev = pd.NaT
            
    return [np.median(tdiff), np.amax(tdiff)] if tdiff else [pd.NaT, pd.NaT]

def calc_median_response_time_diff(messages):
    """ Calculates the median response time per relationship.
    Distinguishes between use to client and client to user. """
    median_response_time_user_to_client = messages.groupby(['client_id','user_id']).apply(calc_median_response_time_user_to_client)
    median_response_time_client_to_user = messages.groupby(['client_id','user_id']).apply(calc_median_response_time_client_to_user)
    median_response_time_user_to_client = pd.DataFrame(median_response_time_user_to_client.values.tolist(),
             columns=['uc_median_response_time','uc_max_response_time'],
             index=median_response_time_user_to_client.index)
    median_response_time_client_to_user = pd.DataFrame(median_response_time_client_to_user.values.tolist(),
             columns=['cu_median_response_time','cu_max_response_time'],
             index=median_response_time_client_to_user.index)
    return pd.concat([median_response_time_user_to_client,median_response_time_client_to_user],axis=1)

# ***** functions dealing with response times (old functions) ************

def calc_median_response_time_per_group(df,grp_names):
    """ Apply a function per defined group. Written for grp_names=['client_id','user_id'] """
    df_grp = df.groupby(grp_names)
    time_diff_key = []
    time_diff_val = []
    for key,group in df_grp:
        tval = calc_median_response_time_per_relation(group)
        time_diff_key.append(key)
        time_diff_val.append(tval)
    return pd.DataFrame(
        pd.Series(time_diff_val,index=pd.MultiIndex.from_tuples(time_diff_key,names=grp_names)),
                  columns=['median_time'])

def calc_median_response_time_per_relation(df_uc):
    """ Calculates the median time per defined relation. """
    if df_uc['time_diff_response'].notnull().any():
        tval = df_uc['time_diff_response'].median()
    else:
        tval = np.nan
    return tval

def find_index_outbound(df):
    index_inbound = df.query('inbound == True').index.values # find inbound indices
    # need to iterate backwards until find entry with 'inbound == False'
    index_outbound = []
    for n in index_inbound:
        for k in range(n, - 1, -1):
            if (df.loc[k,'inbound'] == False) & (df.loc[k,'sent'] == True):
                #print(k,df.loc[k,'inbound'])
                break
        index_outbound.append(k)
    return index_inbound, np.array(index_outbound)

def get_response_timelag(df):
    """ Calculates the time lag between sent and received message. """
    # function requires index manipulation, hence .reset_index is called,
    # which is not ideal with groupby
    df = df.sort_values('send_at').reset_index(drop=True)
    index_inbound, index_outbound = find_index_outbound(df)
    df['time_diff_response'] = pd.Series(df.loc[index_inbound,'send_at'].values - df.loc[index_outbound,'send_at'].values,
                                                 index=index_inbound)
    return df

def calc_median_response_time(df):
    """ Calculates time difference per relation. """
    col_select = ['id','client_id','user_id','body','inbound','send_at','sent']
    messages_time_diff = (df[col_select].groupby(['client_id','user_id'])
                      .apply(get_response_timelag)
                     .reset_index(drop=True))
    return calc_median_response_time_per_group(messages_time_diff,['client_id','user_id']).reset_index()

# ***** functions dealing with scheduling ************

def calc_median_schedule_time_per_group(messages):
    # some times can be negative, will use the absolute value for now
    tdiff = np.abs(messages['send_at'].apply(lambda x: x.round('s')) - messages['created_at'].apply(lambda x: x.round('s')))
    return [np.median(tdiff), np.amax(tdiff)]

def calc_median_schedule_time(messages):
    median_schedule_time = messages.groupby(['client_id','user_id']).apply(calc_median_schedule_time_per_group)
    return (pd.DataFrame(median_schedule_time.values.tolist(),
                         columns=['median_schedule_time','max_schedule_time'],index=median_schedule_time.index))

# ***** functions dealing with finding report/court dates from text messages ************

def estimate_report_dates(messages,col_names):
    """ Estimates dates from text messages and provide an estimate for the next report dates. """
    messages_send_dates = messages[col_names].copy()
    messages_send_dates['dates_ext'] = messages_send_dates.apply(find_dates_in_row,axis=1)['body']
    messages_send_dates['dates_est'] = messages_send_dates.apply(compare_dates_in_row,axis=1)['dates_ext']
    
    return messages_send_dates

def calc_median_report_time_diff_per_group(messages_send_dates):
    """ Calculates the median time difference between
    message send time and estimated report or court dates. """
    
    # reset index to apply operation within group
    messages_send_dates.reset_index(drop=True,inplace=True)
    
    # loop over all estimated dates
    tdiff = []
    for i in range(len(messages_send_dates)):
        if messages_send_dates.loc[i,'dates_est']:
            for date_est in messages_send_dates.loc[i,'dates_est']:
                tdiff.append(date_est - messages_send_dates.loc[i,'send_at'])
                
    return [np.median(tdiff),len(tdiff)] if tdiff else [pd.NaT,0]

def calc_median_report_time_diff(messages):
    """ Calculates the median report time and number of possible report times per relationship. """
    col_select = ['client_id','user_id','send_at','body']
    messages_send_dates = estimate_report_dates(messages,col_select)
    median_report_time = messages_send_dates.groupby(['client_id','user_id']).apply(calc_median_report_time_diff_per_group)
    return pd.DataFrame(median_report_time.values.tolist(),columns=['median_report_time','n_report_time'],index=median_report_time.index)

# ***** functions dealing with topic classification in messages ************

def build_messages_vectorizer():
    """ Compile term frequency vectorizer """
    tf_vectorizer = CountVectorizer(max_df=0.95, min_df=2, max_features=n_features,
                                stop_words='english',
                               token_pattern=r'[a-zA-Z\-][a-zA-Z\-]{2,}')
    return tf_vectorizer

def find_msg_topic(prob):
    """ Categorize messages into topic with the maximum probability.
    Assign 0 for uniform probabilities. """
    if (prob == prob.max()).all():
        return [0,np.median(prob)]
    else:
        return [prob.argmax()+1,prob.max()] # add 1 to the topics to distinguish from 'unclassified' or uniform probability

def find_msg_topic_in_row(row,tf_in_vect,tf_out_vect,lda_in_model,lda_out_model):
    """ Models and vectorizer must be passed in. """
    
    # select trained model depending on it sent or received
    if row['inbound']:
        model = lda_in_model
        tf_vectorizer = tf_in_vect
    else:
        model = lda_out_model
        tf_vectorizer = tf_out_vect
        
    if row.isnull()['body']:
        max_id, max_val  = (0,0)
    else:
        max_id, max_val = find_msg_topic(model.transform(tf_vectorizer.transform([row['body']]))[0])
    return pd.Series({'topic_max':max_id,'topic_maxval':max_val})

def load_topic_model_components(messages):
    """ Loads topic model components into workspace. Must feed in the entire messages table. """
    # loads text vectorizer
    tf_outbound_vectorizer = build_messages_vectorizer()
    tf_inbound_vectorizer = build_messages_vectorizer()
    
    messages_outbound = messages.query('inbound == False')
    messages_inbound = messages.query('inbound == True')
    
    data_samples_outbound = messages_outbound[messages_outbound['body'].notnull()]['body'].values
    data_samples_inbound = messages_inbound[messages_inbound['body'].notnull()]['body'].values
    
    # fit vectorizer to corresponding data
    tf_outbound = tf_outbound_vectorizer.fit_transform(data_samples_outbound)
    tf_inbound = tf_inbound_vectorizer.fit_transform(data_samples_inbound)
    
    # loads models
    lda_outbound_model = pickle.load(open(fname_outbound_topic_model, 'rb'))
    lda_inbound_model = pickle.load(open(fname_inbound_topic_model, 'rb'))
    
    
    return [lda_outbound_model, lda_inbound_model,
            tf_outbound_vectorizer, tf_inbound_vectorizer,
           tf_outbound, tf_inbound]

def print_top_words(model, feature_names, n_top_words):
    for topic_idx, topic in enumerate(model.components_):
        message = "Topic #%d: " % topic_idx
        message += " ".join([feature_names[i]
                             for i in topic.argsort()[:-n_top_words - 1:-1]])
        print(message)
    print()

def show_top_words(model,label='',show_result=False):
    """ Topic ordering is model dependent, which in turn is data dependent. """
    for topic_idx, topic in enumerate(model.components_):
        # arsort() sorts by increasing probabilities
        top_words_idx = topic.argsort()[:-n_top_words - 1:-1]
        top_words = [tf_feature_names[i] for i in top_words_idx.tolist()]
        top_words_freq = np.sort(topic)[:-n_top_words - 1:-1]
    
        # plot results
        plt.figure(figsize=(10,6))
        sns.barplot(y=top_words,x=top_words_freq)
        #plt.xticks(rotation=60)
        plt.ylabel('Word')
        plt.xlabel('Topic '+label+' '+str(topic_idx+1)+'\nWord frequency')
        fout = 'topic_'+label+'_'+str(topic_idx+1)+'.png'
        plt.savefig(fout,dpi=200)
        if show_result: plt.show()
        plt.close()
    
def map_messages_topics(messages,tf_in_vect,tf_out_vect,lda_in_model,lda_out_model):
    """ Determines the topic from each messages.
    Classify sent and received messages according to respective models. """
    
    messages_topics = (messages[['body','inbound']]
                           .apply(lambda row: find_msg_topic_in_row(row,tf_in_vect,tf_out_vect,lda_in_model,lda_out_model),axis=1))
    
    return messages.merge(messages_topics,left_index=True,right_index=True)


def show_messages_sequence(messages_uc,user_id,client_id,show_result=False):
    """ Show message sequence for a relationship. """
    
    # select only a relationship
    messages_uc = messages_uc.query('client_id == @client_id and user_id == @user_id').sort_values('send_at').reset_index(drop=True)
    
    # separate into sent and received
    messages_inbound = messages_uc.query('inbound == True')
    messages_outbound = messages_uc.query('inbound == False')
    
    # plot commands
    plt.figure(figsize=(10,6))
    # +0.01 in the size label is to plot NaN messages
    if 'topic_max' in messages_uc.columns:
        plt.scatter(messages_outbound['send_at'].values,
            messages_outbound['topic_max'].values,
            s=(messages_outbound['topic_maxval']+0.01)*200,c='red',label='sent',alpha=0.4)#,c=messages_sample['inbound'].values)
        plt.scatter(messages_inbound['send_at'].values,
            messages_inbound['topic_max'].values,
            s=(messages_inbound['topic_maxval']+0.01)*200,c='blue',label='received',alpha=0.4)
    plt.plot(messages_uc['send_at'].values,messages_uc['topic_max'].values,
             marker='',linestyle='-',color='black',alpha=0.4)
    
    if 'dates_est' in messages_uc.columns:
        for elem in messages_uc['dates_est']:
            for xc in elem:
                plt.axvline(x=xc,color='green',alpha=0.4) # plot vertical lines for estimated court dates
    plt.xlabel('Date')
    plt.ylabel('Topic')
    plt.legend()
    plt.xlim([messages_uc['send_at'].min()-pd.Timedelta(days=2), messages_uc['send_at'].max()+pd.Timedelta(days=2)])
    plt.title('user: ' + str(user_id) + ' client: ' + str(client_id))
    plt.savefig('messages_topics_u'+str(user_id)+'_c'+str(client_id)+'.png',dpi=200)
    if show_result: plt.show()
    plt.close()

def map_messages_topics_list(messages,uc_list,estimate_date=False):
    """ Maps messages topics for a list of user and client.
    Outputs corresponding messages and plot. """
    
    # loads topic model
    [lda_outbound_model, lda_inbound_model,
            tf_outbound_vectorizer, tf_inbound_vectorizer,
           tf_outbound, tf_inbound] = load_topic_model_components(messages)
    
    for client_id, user_id in uc_list:
        messages_sample = messages.query('client_id == @client_id and user_id == @user_id').sort_values('send_at').reset_index(drop=True)
        messages_sample = map_messages_topics(messages_sample,
                    tf_inbound_vectorizer,tf_outbound_vectorizer,
                    lda_inbound_model,lda_outbound_model)
        messages_sample[['inbound','send_at','body','topic_max','topic_maxval']].to_csv('messages_u'+str(user_id)+'_c'+str(client_id)+'.csv',index=False)
        
        # add annotations for estimated report/court dates
        if estimate_date:
            messages_sample = estimate_report_dates(messages_sample,messages_sample.columns)
            
        show_messages_sequence(messages_sample,user_id,client_id,show_result=False)


def count_topics_excluding_per_group(messages_topics,exclude_list=[]):
    """ Counts number of topics that are not in the exclude_list. """
    return len(set(exclude_from_list(messages_topics['topic_max'],exclude_list)))

def calc_mode_topic_per_group(messages_topics):
    """ Calculates the most frequent topic. Also, takes into account the probability. """
    if messages_topics.isnull()['topic_max'].any():
        return np.nan
    
    mode_val = messages_topics['topic_max'].mode().values
    
    if len(mode_val) == 1:
        return mode_val[0]
    elif len(mode_val) > 1:
        mode_val_mask = messages_topics['topic_max'].isin(mode_val)
        mode_val_loc = messages_topics[mode_val_mask]['topic_maxval'].idxmax()
        mode_val = messages_topics.loc[mode_val_loc,'topic_max']
    elif len(mode_val) == 0:
        mode_val = np.nan

    return mode_val

def calc_median_topics(messages,outbound_exclude_list=[],inbound_exclude_list=[]):
    """ Calculates the median topic values per conversation.
    Returns inbound/outbound median topics, number of topics, and number of topics excluding specified topics. """
    [lda_outbound_model, lda_inbound_model,
            tf_outbound_vectorizer, tf_inbound_vectorizer,
           tf_outbound, tf_inbound] = load_topic_model_components(messages)
    messages = map_messages_topics(messages,
                    tf_inbound_vectorizer,tf_outbound_vectorizer,
                    lda_inbound_model,lda_outbound_model)
        
    return pd.concat(join='outer',axis=1)

    outbound_topics_median = messages.query('inbound == False').groupby(['client_id','user_id'])['topic_max'].agg(['median','nunique'])
    outbound_topics_median.columns = ['outbound_median_topic','outbound_ntopics']
    outbound_ntopics_exclude = pd.DataFrame(messages.query('inbound == False').groupby(['client_id','user_id'])
                              .apply(lambda x: count_topics_excluding_per_group(x,outbound_exclude_list)),
                                          columns=['outbound_ntopics_exclude'])
    inbound_topics_median = messages.query('inbound == True').groupby(['client_id','user_id'])['topic_max'].agg(['median','nunique'])
    inbound_topics_median.columns = ['inbound_median_topic','inbound_ntopics']
    inbound_ntopics_exclude = pd.DataFrame(messages.query('inbound == True').groupby(['client_id','user_id'])
                              .apply(lambda x: count_topics_excluding_per_group(x,inbound_exclude_list)),
                                         columns=['inbound_ntopics_exclude'])
    return pd.concat([outbound_topics_median,outbound_ntopics_exclude,
                      inbound_topics_median,inbound_ntopics_exclude], join='outer',axis=1)

def calc_mode_topics(messages):
    """ Calculates the mode topice values per conversation. """
    [lda_outbound_model, lda_inbound_model,
            tf_outbound_vectorizer, tf_inbound_vectorizer,
           tf_outbound, tf_inbound] = load_topic_model_components(messages)
    messages = map_messages_topics(messages,
                    tf_inbound_vectorizer,tf_outbound_vectorizer,
                    lda_inbound_model,lda_outbound_model)
    
    outbound_topics_mode = pd.DataFrame(messages.query('inbound == False').groupby(['client_id','user_id'])
                              .apply(calc_mode_topic_per_group), columns=['outbound_mode_topic'])
    outbound_ntopics = messages.query('inbound == False').groupby(['client_id','user_id'])['topic_max'].agg(['nunique'])
    outbound_ntopics.columns = ['outbound_ntopics']
    inbound_topics_mode = pd.DataFrame(messages.query('inbound == True').groupby(['client_id','user_id'])
                              .apply(calc_mode_topic_per_group), columns=['inbound_mode_topic'])
    inbound_ntopics = messages.query('inbound == True').groupby(['client_id','user_id'])['topic_max'].agg(['nunique'])
    inbound_ntopics.columns = ['inbound_ntopics']

    return pd.concat([outbound_topics_mode,outbound_ntopics,inbound_topics_mode,inbound_ntopics],join='outer',axis=1)

# ***** functions for supervision failure classification ************

def downsample_majority_class_to_sf(df):
    """ Due to the imbalance data set. Downsample the majority class into two categories.
    Two categories: supervision_failure == [True,False] """
    
    # Separate majority and minority classes
    df_majority = df.query('supervision_failure == False')
    df_minority = df.query('supervision_failure == True')
    
    # Downsample majority class
    df_majority_downsampled = resample(df_majority, 
                                 replace=False,    # sample without replacement
                                 n_samples=len(df_minority))     # to match minority class
                                 #random_state=123) # reproducible results
    
    # Combine minority class with downsampled majority class
    return pd.concat([df_majority_downsampled, df_minority])

def print_feature_importance(tree_model,features):
    """ Prints out the predictive features. """
    # http://scikit-learn.org/stable/auto_examples/ensemble/plot_forest_importances.html
    importances = tree_model.feature_importances_
    std = np.std([tree.feature_importances_ for tree in tree_model.estimators_], axis=0)
    indices = np.argsort(importances)[::-1]

    # Print the feature ranking
    print("Feature ranking:")

    for f in range(len(features)):
        print("{:d}. feature {:d}: {} ({:f} +/- {:f})"
              .format(f + 1, indices[f], 
                      features[indices[f]], importances[indices[f]], std[indices[f]]))

def scan_prob_each_feature(model,X,h,features,fidx,show_result=False,save_result=False):
    """ Calculates the probabilities by changing each feature value for all observations.
    Requires externally defined step sizes.
    Plots the ['mean','1st percentile','5th percentile','10th percentile','25th percentile','median'] """
    
    Xmax = np.amax(X,axis=0)
    Xmin = np.amin(X,axis=0)

    X_copy = np.copy(X)

    print('Processing: ', features[fidx])
    
    xf = np.arange(Xmin[fidx],Xmax[fidx],h[fidx]) # range of feature values
    yf = np.zeros((len(xf),6))
    yf_arr = np.zeros((X.shape[0],len(xf)))
    
    # loop over each feature value
    for i in range(len(xf)):
        X_copy[:,fidx] = xf[i] # assign that feature value for all observations for that feature
        yprobi = model.predict_proba(X_copy)[:,0] # calculate probabilities
        yf[i,:] = [np.mean(yprobi),np.percentile(yprobi,1),np.percentile(yprobi,5),np.percentile(yprobi,10),np.percentile(yprobi,25),np.median(yprobi)]
        yf_arr[:,i] = yprobi
    
    # plot result
    if save_result | show_result:
        plt.figure(figsize=(16,10))
        plt.plot(xf,yf)
        for i in range(len(xf)):
            plt.scatter(xf[i]*np.ones(X.shape[0]),yf_arr[:,i],c='gray',alpha=0.2) # need to change to swarm plot for better visualization!
        plt.xlabel(features[fidx])
        plt.ylabel('Probabilities')
        plt.legend(['mean','1st percentile','5th percentile','10th percentile','25th percentile','median'])
        if save_result: plt.savefig('model_prob_'+features[fidx]+'.png',dpi=200)
        if show_result: plt.show()
        plt.close()
        
    return xf, yf, yf_arr

# ***** functions for word count ************

def find_wordcount_in_string(string):
    """ Calculates word counts in a string. """
    if isinstance(string,str):
        return len(string.split())
    else:
        return 0

def find_wordcount_per_group(messages):
    """ Calculates word counts in a group.
    Returns the median and maximum values. """
    messages = messages.sort_values('send_at',ascending=True).reset_index(drop=True)
    
    wcount = []
    for i in range(len(messages)):
        wcount.append(find_wordcount_in_string(messages.loc[i,'body']))
        
    return [np.median(wcount), np.amax(wcount)] if wcount else [0,0]

def find_wordcount(messages):
    """ Counts words in text messages. """
    outbound_median_wordcount = messages.query('inbound == False').groupby(['client_id','user_id']).apply(find_wordcount_per_group)
    inbound_median_wordcount = messages.query('inbound == True').groupby(['client_id','user_id']).apply(find_wordcount_per_group)
    
    outbound_counts = pd.DataFrame(outbound_median_wordcount.values.tolist(),
                            columns=['outbound_median_wordcount','outbound_max_wordcount'],
                                   index=outbound_median_wordcount.index)
    inbound_counts = pd.DataFrame(inbound_median_wordcount.values.tolist(),
                                 columns=['inbound_median_wordcount','inbound_max_wordcount'],
                                 index=inbound_median_wordcount.index)
    
    return pd.concat([outbound_counts,inbound_counts],axis=1).fillna(0)

# ***** functions for significance testing ************

def permutation_test_diff_means(x,y,nperm=10000):
    """ Calculates the p-value for the difference in means. """
    # Compute difference of means
    empirical_diff_means = diff_of_means(x, y)

    # Draw permutation replicates
    perm_replicates = draw_perm_reps(x, y,
                                 diff_of_means, size=nperm)

    # Compute p-value
    p = np.sum(perm_replicates >= empirical_diff_means) / len(perm_replicates)
    
    return p

def distribution_test_diff_means(x,y,nperm=10000):
    """ Use the difference of means to test if data comes from same distribution. """
    # Compute difference of means
    empirical_diff_means = diff_of_means(x, y)

    xy_concat = np.concatenate((x, y))

    # Initialize bootstrap replicates
    bs_replicates = np.empty(nperm)

    for i in range(nperm):
        # Generate bootstrap sample
        bs_sample = np.random.choice(xy_concat, size=len(xy_concat))
    
        # Compute replicate
        bs_replicates[i] = diff_of_means(bs_sample[:len(x)],
                                     bs_sample[len(x):])

    # Compute and print p-value: p
    p = np.sum(bs_replicates >= empirical_diff_means) / len(bs_replicates)
    
    return p

        
 # ***** functions for sentiment extraction ************

def find_sentiment_in_string(string):
    if isinstance(string,str):
        return list(TextBlob(string).sentiment[:])
    else:
        return [np.nan,np.nan]

def find_sentiment_in_group(messages):
    """ Find median, minimum, and maximum sentiments per relationship. """
    pol = []
    sub = []
    for message in messages['body']:
        polt,subt = find_sentiment_in_string(message)
        pol.append(polt)
        sub.append(subt)
    
    if np.isnan(polt).all():
        return [np.nan, np.nan, np.nan, np.nan, np.nan]
    else:
        return [np.nanmedian(pol),np.nanmedian(sub),np.nanmin(pol),np.nanmax(pol),np.nanmax(sub)]

def find_sentiment(messages):
    median_sentiments = messages.groupby(['client_id','user_id']).apply(find_sentiment_in_group)
    return pd.DataFrame(median_sentiments.values.tolist(),
                        columns=['median_polarity','median_subjectivity','min_polarity','max_polarity','max_subjectivity'],
                       index=median_sentiments.index)
