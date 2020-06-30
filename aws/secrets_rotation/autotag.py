import boto3
import json
import logging
import os
import hashlib

logger = logging.getLogger()
logger.setLevel(logging.INFO)


def lambda_handler(event, context):
    service_client = boto3.client('secretsmanager', endpoint_url=os.environ['SECRETS_MANAGER_ENDPOINT'])
    arn = event['detail']['additionalEventData']['SecretId']
    
    secret = service_client.get_secret_value(SecretId=arn)
    logger.info(f'received event for {secret["Name"]}')

    m = hashlib.sha256()
    m.update(bytes(secret['Name'], 'utf-8'))
    #TODO check whether it's SecretString or SecretBinary
    m.update(bytes(secret['SecretString'], 'utf-8'))
    sha256sum = m.hexdigest()

    response = service_client.tag_resource(
        SecretId=arn,
        Tags=[
            {
                'Key': 'hash',
                'Value': sha256sum,
            }
        ]
    )

