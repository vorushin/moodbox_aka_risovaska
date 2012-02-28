-define(AWS_KEY, "your_key").
-define(AWS_SEC_KEY, "your_sec_key").
-define(AWS_SECURE, true).
%-define(LOCAL_REPOSITORY, true). % switch between local repository and S3
-define(LOCAL_DIRECTORY, "priv/s3/").
-define(BUCKET, "moodbox-server-data").

-define(ARTMESSAGE_PREFIX, "artmessage/").
-define(USERPIC_PREFIX, "userpic/").
-define(CHANNEL_LOGO_PREFIX, "channel_logo/").
-define(MOODSTRIP_PREFIX, "moodstrip/").
-define(AWS_S3_HOST, "s3.amazonaws.com").
-define(S3_LAST_MODIFIED_HEADER, "last-modified").
-define(S3_USER_METADATA_HEADER, "x-amz-meta-data").

% S3 counters
-define(MOODSTRIP_ITEM_COUNTER, moodstrip_item).
-define(ART_MESSAGE_COUNTER, art_message).
