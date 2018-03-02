MANAGE_PY := ${ENVSITEPACKAGESDIR}/manage.py

emacs:
	cask emacs -Q -l scripts/init.el /docker:root@olympia_web_1:/code/manage.py

install:
	cask install

update:
	cask update

clone:
	git clone https://github.com/mozilla/addons-server testproject

start:
	docker-compose -p olympia -f testproject/docker-compose.yml up web

stop:
	docker-compose -p olympia -f testproject/docker-compose.yml down
	docker volume prune -f
	docker network prune -f

toxinit: ${MANAGE_PY}

${MANAGE_PY}:
	test -n "${ENVSITEPACKAGESDIR}"
	env -u DJANGO_SETTINGS_MODULE django-admin startproject testproject ${ENVSITEPACKAGESDIR}
	touch $@
