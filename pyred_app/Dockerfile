FROM ubuntu
MAINTAINER foo <foo@bar.com>

RUN apt-get update
RUN apt-get install -y python python-pip
RUN pip install redis flask

ADD ./hello.py /hello.py
EXPOSE 8000
CMD ["python", "/hello.py"]
