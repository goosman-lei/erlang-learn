ROOT = $(@D)
LIB_DIR = ${ROOT}/lib
SRC_DIR = ${ROOT}/src


CC = erlc -o ${LIB_DIR}
ERL = erl -noshell -pa ${LIB_DIR}
ESCRIPT = escript

all: prepare \
	done

demo.beam:
	@echo "================================== Demo 1: demo ====================================="
	${CC} ${SRC_DIR}/demo.erl
	${ERL} -s demo start
	@echo

# 路径准备及清理
prepare:
	@mkdir -p ${LIB_DIR}
clean:
	find ${LIB_DIR} -name *.beam -exec rm -rf {} \;
done:
	@echo Welldone
