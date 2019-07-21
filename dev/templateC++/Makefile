release:
	@mkdir -p build bin
	@cd build && cmake -DCMAKE_BUILD_TYPE=Release .. && make

debug:
	@mkdir -p build debug
	@cd build && cmake -DCMAKE_BUILD_TYPE=Debug .. && make

clean:
	@rm -rf bin
	@rm -rf build
	@rm -rf debug
