.PHONY: build restart build-and-restart-server

build:
	lake build
run:
	./build/bin/lean_server
load_test:
	wrk -t12 -c40 -d10s http://localhost:8081
kill:
	pkill lean_server || true

build-and-restart-server: build kill

watch-build:
	watchman-make -p LeanServer/*.lean Main.lean views/*.lean -t build-and-restart-server 
