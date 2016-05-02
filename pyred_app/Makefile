VERSION=current
HOST=10.0.2.15

all: build deploy

build:
	docker build -t myusername/helloworld .
	docker tag -force myusername/helloworld $(HOST):5000/myusername/helloworld:$(VERSION)

deploy:
	docker push $(HOST):5000/myusername/helloworld:$(VERSION)
	ssh vagrant@$(HOST) ./crashburn/pyred_app/deploy-app myusername/helloworld $(VERSION)
