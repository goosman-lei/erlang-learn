ROOT = $(@D)
LIB_DIR = ${ROOT}/lib
SRC_DIR = ${ROOT}/src


CC = erlc -o ${LIB_DIR}
ERL = erl -noshell -pa ${LIB_DIR}
ESCRIPT = escript

all: prepare \
	spawn.beam \
	area_server.beam \
	calc_spawn_rusage.beam \
	flush_buffer.beam \
	timer.beam \
	exercise_1.beam \
	exercise_2.beam \
	done

spawn.beam:
	@echo "================================== Demo 1: spawn ====================================="
	${CC} ${SRC_DIR}/spawn.erl
	${ERL} -s spawn start
	@echo

area_server.beam:
	@echo "================================== Demo 2: area server ====================================="
	${CC} ${SRC_DIR}/area_server.erl
	${ESCRIPT} ${SRC_DIR}/area_server_main.erl
	@echo

calc_spawn_rusage.beam:
	@echo ================================== Demo 3: area server =====================================
	${CC} ${SRC_DIR}/calc_spawn_rusage.erl
	${ERL} -p 1000000 -s calc_spawn_rusage start
	@echo

flush_buffer.beam:
	@echo ================================== Demo 4: flush buffer =====================================
	${CC} ${SRC_DIR}/flush_buffer.erl
	${ERL} -s flush_buffer start
	@echo

timer.beam:
	@echo ================================== Demo 5: timer =====================================
	${CC} ${SRC_DIR}/timer.erl
	${ERL} -s timer start
	@echo

clock.beam:
	@echo ================================== Demo 6: clock =====================================
	${CC} ${SRC_DIR}/clock.erl
	${ERL} -s clock start
	@echo

ctemplate.beam:
	@echo ================================== Demo 7: ctemplate =====================================
	${CC} ${SRC_DIR}/ctemplate.erl
	${ERL} -s ctemplate start
	@echo

exercise_1.beam:
	@echo ================================== Demo 8: exercise_1 =====================================
	${CC} ${SRC_DIR}/exercise_1.erl
	${ERL} -s exercise_1 start
	@echo

exercise_2.beam:
	@echo ================================== Demo 9: exercise_2 =====================================
	${CC} ${SRC_DIR}/exercise_2.erl
	${ERL} -s exercise_2 start
	@echo

# 路径准备及清理
prepare:
	@mkdir -p ${LIB_DIR}
clean:
	find ${LIB_DIR} -name *.beam -exec rm -rf {} \;
done:
	@echo Welldone
