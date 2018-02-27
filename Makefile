emacs:
	cask emacs -Q -l scripts/init.el /docker:root@olympia_web_1:/code/manage.py

install:
	cask install

update:
	cask update

testproject_clone:
	git clone https://github.com/mozilla/addons-server testproject

testproject_start:
	docker-compose -p olympia -f testproject/docker-compose.yml up web

testproject_stop:
	docker-compose -p olympia -f testproject/docker-compose.yml down
	docker volume prune -f
	docker network prune -f
