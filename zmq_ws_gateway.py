import  zmq
import  time
import  logging
from    zmq.eventloop import ioloop, zmqstream
# we use this to merge the ioloop from zmq and tornado
ioloop.install()

import  tornado.websocket
import  tornado.web
import  tornado.httpserver
import  tornado.ioloop

import  uuid
import  json

###
# set up logging to stdout
import sys
root_logger = logging.getLogger()
root_logger.setLevel(logging.INFO)
stream_handler = logging.StreamHandler(sys.stdout)
stream_handler.setFormatter(logging.Formatter(logging.BASIC_FORMAT))
root_logger.addHandler(stream_handler)

###
# dice rolling logic
import  re
import  random
dice_expr = re.compile('^(\d+)d(\d+)$')
def _roll(what):
  g = dice_expr.match(what)
  if g:
    num_rolls, dice_size = g.groups()
    num_rolls = int(num_rolls)
    dice_size = int(dice_size)
    return [(random.randrange(1, dice_size)) for _ in range(num_rolls)]
  else:
    return 'whaaat?'

###
# server
def server(port='5556'):
  context = zmq.Context()
  socket = context.socket(zmq.REP)
  socket.bind('tcp://*:{0}'.format(port))
  logging.info('[SV] running R.O.L.L. server on port {port}'.format(port=port))

  while True:
    clientid, clientreq = socket.recv_string().split()
    logging.info('[SV] received roll {req} from client {cl}'.format(req=clientreq, cl=clientid))

    time.sleep(2) # so we can show-case how requests are queued up
    roll = json.dumps(_roll(clientreq)) # this is bad - we should sanitise the input
    socket.send_string('{cl}:{r}'.format(cl=clientid, r=roll)) # sending the clientid back because the gateway doesn't know whose this belongs to

class PubWebSocket(tornado.websocket.WebSocketHandler):
  clients = dict()
  zme_socket = None

  def open(self):
    client_id = str(uuid.uuid1())
    PubWebSocket.clients[client_id] = self
    self.client_id = client_id
    logging.info('[PWS] websocket opened - request headers: \n{headers}'.format(headers=self.request.headers))
    logging.info('client id: {clid}'.format(clid=client_id))

  def on_message(self, msg):
    logging.info('[PWS] received message {msg} from client {cl}'.format(msg=msg, cl=self.client_id))
    PubWebSocket.zmq_socket.send_string('{0} {1}'.format(self.client_id, msg))

  @classmethod
  def publish_updates(cls, msgs):
    logging.info('[PWS] receives msgs <{msgs}>'.format(msgs=msgs))
    for msg in msgs:
      msg_unicode = msg.decode('utf-8') # given we're receiving bytes
      cid, upd = msg_unicode.split(':')
      logging.info('[PWS] publishing {msg} to {cl}'.format(msg=upd, cl=cid))
      client = PubWebSocket.clients.get(cid)
      if client:
        client.write_message(u'{upd}'.format(upd=upd)) # note the unicode!
      else:
        logging.error('[PWS] received callback for nonexsitent client: {cl}'.format(cl=cid))

  def on_close(self):
    del PubWebSocket.clients[self.client_id]
    logging.info('[PWS] removing client {cl}'.format(cl=self.client_id))

if __name__ == '__main__':
  server_port = '5556'
  import threading
  t = threading.Thread(target=server, args=(server_port,))
  t.start()

  app = tornado.web.Application([
    (r'/ws', PubWebSocket), # this endpoint will be localhost/ws
    ])
  http_server = tornado.httpserver.HTTPServer(app)
  http_server.listen(8088)

  context = zmq.Context()
  client_sock = context.socket(zmq.REQ)
  client_sock.connect('tcp://localhost:{port}'.format(port=server_port))
  stream_sock = zmqstream.ZMQStream(client_sock)
  stream_sock.on_recv(PubWebSocket.publish_updates)
  PubWebSocket.zmq_socket = stream_sock

  tornado.ioloop.IOLoop.instance().start() # get the ball rolling
