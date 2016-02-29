#!/bin/bash
#
# This script is intended for automatically grade solutions created by students
# to problems of the pl-man environment.
#

###
### Variables
###

# Numero de ejecuciones por fase
#
N_EJECS[0]=1				# NÃºmero de ejecuciones a realizar en fase 0
N_EJECS[1]=3				# NÃºmero de ejecuciones a realizar en fase 1
N_EJECS[2]=5				# NÃºmero de ejecuciones a realizar en fase 2
N_EJECS[3]=15				# NÃºmero de ejecuciones a realizar en fase 3
N_EJECS[4]=25				# NÃºmero de ejecuciones a realizar en fase 4
N_EJECS[5]=0				# NÃºmero de ejecuciones a realizar en fase 4
N_EJECS[6]=3				# NÃºmero de ejecuciones a realizar en fase 4

# Numero de ejecuciones por fase
#
N_MAXMOVS[0]=50			# Número maximo de movimientos a realizar en fase 0
N_MAXMOVS[1]=350			# Número maximo de movimientos a realizar en fase 0
N_MAXMOVS[2]=500			# Número maximo de movimientos a realizar en fase 0
N_MAXMOVS[3]=750			# Número maximo de movimientos a realizar en fase 0
N_MAXMOVS[4]=1250			# Número maximo de movimientos a realizar en fase 0
N_MAXMOVS[5]=0				# Número maximo de movimientos a realizar en fase 0
N_MAXMOVS[6]=750			# Número maximo de movimientos a realizar en fase 0

# Timeouts de ejecucion por fase
#
T_TIMEOUT[0]=2				# Tiempo mÃ¡ximo de ejecuciÃ³n para fase 0
T_TIMEOUT[1]=3				# Tiempo mÃ¡ximo de ejecuciÃ³n para fase 1
T_TIMEOUT[2]=4				# Tiempo mÃ¡ximo de ejecuciÃ³n para fase 2
T_TIMEOUT[3]=4				# Tiempo mÃ¡ximo de ejecuciÃ³n para fase 3
T_TIMEOUT[4]=7				# Tiempo mÃ¡ximo de ejecuciÃ³n para fase 4
T_TIMEOUT[5]=5				# Tiempo mÃ¡ximo de ejecuciÃ³n para fase 4
T_TIMEOUT[6]=4				# Tiempo mÃ¡ximo de ejecuciÃ³n para fase 4

# Penalizaciones y notas
#
NOTA[0]=0					# Nota de la primera ejecuciÃ³n
MOVIMIENTOS[0]=0			# Numero de movimientos en primera ejecucion
INFERENCIAS[0]=0			# Numero de inferencias en primera ejecucion
TIEMPOCPU[0]=0				# Tiempo de CPU tardado para primera ejecucion
POR_WARNING=0.25			# PenalizaciÃ³n por cada warning
POR_COLISION=0.25			# PenalizaciÃ³n por cada colisiÃ³n contra un sÃ³lido
POR_INTENTO=0.3			# PenalizaciÃ³n por cada intento infructuoso de acciÃ³n
POR_ACCION_ERRONEA=0.5	# PenalizaciÃ³n por cada acciÃ³n desconocida
POR_FALLO_DE_REGLA=0.75 # PenalizaciÃ³n por cada fallo de regla
POR_MAX_MOVIMIENTOS=0.5 # PenalizaciÃ³n por superar el mÃ¡ximo nÃºmero de movimientos permitido

# Control del funcionamiento del script
#
MAIN_FOLDER=grades		# Ruta de la carpeta principal donde ubicar las correcciones
ALUMNO=desconocido		# Nombre del alumno que estÃ¡ siendo corregido
ALU_ACRO=des			   # Acronimo del alumno (username)
FASE=3						# Fase en correccion
MAPA_ALU=1					# Numero de mapa que tiene el alumno (1,2,3)
MAPA_REAL=01				# Numero real que tiene el mapa del alumno (00-50)
COMPILE_SH=./c.sh			# Nombre del script de compilaciÃ³n
SOL_FILE=../../2008_2009/practicas/mapas/fase_3/01/solucion.pl	# Archivo con las reglas para resolver el mapa
RULE=mapa3_01_fran			# Por defecto no hay regla de ejecucion
MAX_MOVEMENTS=1000		# Movimientos mÃ¡ximos para el mapa
WARN=0						# Warnings encontrados al compilar
FECHA=`date`            # Fecha del sistema
ALTERNATE_MAP_FILE=""	# Fichero alternativo de mapa (por si no estÃ¡ en la ruta estÃ¡ndar)
EMAIL_ADDRESS=no			# Dirección de email del alumno para enviarle mail con los resultados, en caso de que se quiera

# Extensiones para ficheros de salida
#
DBIN=bin				# Fichero binario de salida
DTMP=temp				# Fichero temporal de compilación
DRES=results				# Fichero de resultado de una ejecución
DLOG=log				# Fichero de log de una ejecución
DSUM=summary				# Fichero resumen de corrección de la práctica
DMAI=email				# Fichero con el email enviado al alumno
RESULTS_FILE=out_results.txt		# Archivo de resultados para ser interpretado por "papa"


##
## Función principal del script
##
main()
{
	# Primero, procesar parÃ¡metros de entrada
	processparameters "$@"

	# AdministraciÃ³n de las carpetas del sistema
	create_folder $MAIN_FOLDER
	create_folder $MAIN_FOLDER/$ALU_ACRO
	create_folder $MAIN_FOLDER/$ALU_ACRO/$FASE-$MAPA_ALU
	FOLDER="$MAIN_FOLDER/$ALU_ACRO/$FASE-$MAPA_ALU"
	RESULTS_FILE="$FOLDER/$RESULTS_FILE"
	EXE="$FOLDER/$ALU_ACRO-$FASE-$MAPA_ALU"
	rm -rf "$FOLDER/*" &> /dev/null

	# Llamamos a la funcion principal, guardándonos toda la salida en una variable
	# Después, imprimimos la variable por la salida standard
	#
	calculate_grade "$@" 2> /dev/null > $EXE.$DSUM
	printf "%s\n" "`cat $EXE.$DSUM`"

	# Finalmente, enviamos la salida por correo al alumno, si se nos ha especificado que lo hagamos
	if [ "$EMAIL_ADDRESS" != "no" ]; then
		create_email > $EXE.$DMAI
		mail -a "From: Autocorreccion de practicas de LC <noreply@prolog.i3a.ua.es>" -a "Content-Type: text/html" -s "[Logica] Correccion de practica entregada" "$EMAIL_ADDRESS" < $EXE.$DMAI
	fi
}

##
## Función que ejecuta y calcula notas, que hace todo el trabajo más importante 
##
function calculate_grade()
{
	# Nombre del fichero de solucion sin ruta
	# (Sustituimos por nada, todos los carácteres hasta la última /)
	local SOL_FILE_NAME=${SOL_FILE/#*\/}  

	#
	# Mensajes iniciales
	#
	echo "|=============================================================="
	echo "|** SISTEMA AUTOMATICO DE CORRECCION DE PRACTICAS 2008/2009"
	echo "|=============================================================="
	echo "|> ALUMNO: $ALUMNO (ACRONIMO: $ALU_ACRO)"
	echo "|> MAPA: $FASE - $MAPA_ALU"
	echo "|> FECHA: $FECHA"
	echo "|=============================================================="
	echo 
	echo "_______________________________________________________________"
	echo "| DETALLE DE LA CORRECCION AUTOMATICA"
	echo "|--------------------------------------------------------------"
	echo "| COMPILACION DE LA PRACTICA"
	echo "| "
	echo "| Compilando..."
	echo "|"

	#
	# Compilar y comprobar resultado
	#
	$COMPILE_SH $FASE $MAPA_REAL $RULE -n -o $EXE.$DBIN -t $EXE.$DTMP -l $EXE.$DLOG -s $SOL_FILE -m $MAX_MOVEMENTS $ALTERNATE_MAP_FILE > /dev/null 
	if [ $? != 0 ]; then
		echo "| ATENCION !!!!!!!!!!!!!!!!!!!"
		echo "|"
		echo "| Se han producido errores de compilacion. "
		echo "| La practica no puede ser evaluada mientras no corrijas estos"
		echo "| errores. Consulta el log de compilacion para mas detalles. "
		echo "| Una vez hayas corregido tus errores, vuelve a entregar la "
		echo "| practica. "
		echo "|______________________________________________________________"
		echo
		print_final_results "no_compila" "0" "0" "-1" "-1" "-1" > "$RESULTS_FILE"
		remove_paths_from_error_messages $EXE.temp $SOL_FILE_NAME
		return 1
	elif [[ -e $EXE.$DTMP ]]; then
		WARN=`cat $EXE.$DTMP | egrep -i "Warning" | wc -l`
		echo "| ATENCION !!!!!!!!!!!!!!!!!!!!"
		echo "|"
		echo "| Tu practica contiene $WARN Warning(s)"
		printf "| Cada warning supone %.2f puntos menos en tu nota \n" ${POR_WARNING/\./\,}
		echo "| global. Consulta el log de compilacion para poder identificar"
		echo "| y corregir el/los que haya en tu practica. "
		echo "|______________________________________________________________"
		remove_paths_from_error_messages $EXE.$DTMP $SOL_FILE_NAME
	else
		echo "| Practica compilada sin problemas :) "
		echo "|______________________________________________________________"
		echo "Practica compilada sin problemas" > $EXE.$DTMP
	fi
	
	#
	# EjecuciÃ³n
	#
	echo "|--------------------------------------------------------------"
	echo "| EJECUCION DE LA PRACTICA"
	echo "| "
	echo "|   NoEjecucion   Movimientos   Inferencias   Tiempo      NOTA"
	echo "|   ~~~~~~~~~~~   ~~~~~~~~~~~   ~~~~~~~~~~~   ~~~~~~      ~~~~"

	#
	# Ejecutar N veces y calcular nota de cada una de esas ejecuciones
	#
	local _EFICIENCIA_VALIDA=1	# Si ocurren errores, los resultados de eficiencia serÃ¡ invÃ¡lidos por los errores
	L=${N_EJECS[$FASE]}
	for A in $(seq 1 $L); do
		echo -n "|"
		printf -v _TMP "$A de $L"
		print_right "$_TMP" 13
		execute_with_timeout $EXE.$DBIN ${T_TIMEOUT[$FASE]} $EXE.$DRES
		local _RET=$?
		if [ "$_RET" == "0" ]; then
			# EJECUCION CORRECTA
			sed "s/^.*H.*J\=/\=/" $EXE.$DRES &> $EXE-$A.$DRES
			rm $EXE.$DRES
			calculate_execution_mark $EXE-$A.$DRES $A
		elif [ "$_RET" == "138" ]; then
			# TIMEOUT
			_EFICIENCIA_VALIDA=0
			print_right "Tiempo limite de ejecucion sobrepasado! 0.00" 48
			echo
			NOTA[$A]=0
			mv $EXE.$DRES $EXE-$A.$DRES
		else
			# ERROR DE EJECUCION
			_EFICIENCIA_VALIDA=0
			print_right "Error de ejecucion!                     0.00" 48
			echo
			NOTA[$A]=0
			mv $EXE.$DRES $EXE-$A.$DRES
		fi
		mv $EXE.log $EXE-$A.$DLOG
	done
	rm $EXE.$DBIN
	
	#
	# Calcular notas totales, medias
	#
	local T_NOTA=0
	local T_MOVS=0
	local T_INFS=0
	local T_CPU=0
	for A in $(seq 1 $L); do
		T_NOTA=$(echo "scale=3; $T_NOTA + ${NOTA[$A]}" | bc)
		if (( $_EFICIENCIA_VALIDA )); then
			T_MOVS=$(($T_MOVS + ${MOVIMIENTOS[$A]:=99999}))
			T_INFS=$(($T_INFS + ${INFERENCIAS[$A]:=999999999}))
			T_CPU=$(echo "scale=3; $T_CPU + ${TIEMPOCPU[$A]:=999}" | bc)
		fi
	done
	
	# Si la eficiencia no es vÃ¡lida, estos valores tampoco
	if (( ! $_EFICIENCIA_VALIDA )); then
		T_MOVS=-1
		T_INFS=-1
		T_CPU=-1
	fi

	#
	# Mostrar notas totales
	#
	echo "|--------------------------------------------------------------"
	printf "|    TOTALES:"
	printf -v _TMP "%2.2f" ${T_MOVS/\./\,}
	print_right $_TMP 15
	printf -v _TMP "%2.2f" ${T_INFS/\./\,}
	print_right $_TMP 15
	printf -v _TMP "%2.2f" ${T_CPU/\./\,}
	print_right $_TMP 9
	printf -v _TMP "%2.2f" ${T_NOTA/\./\,}
	print_right $_TMP 10
	echo

	#
	# Calcular notas medias
	#
	T_NOTA=$(echo "scale=3; $T_NOTA / $L" | bc)
	if (( $_EFICIENCIA_VALIDA )); then
		T_MOVS=$(echo "scale=3; $T_MOVS / $L" | bc)
		T_INFS=$(echo "scale=3; $T_INFS / $L" | bc)
		T_CPU=$(echo "scale=3; $T_CPU / $L" | bc)
	fi

	#
	# Imprimir notas medias
	#
	echo "|--------------------------------------------------------------"
	printf "|     MEDIAS:"
	printf -v _TMP "%.2f" ${T_MOVS/\./\,}
	print_right $_TMP 15
	printf -v _TMP "%.2f" ${T_INFS/\./\,}
	print_right $_TMP 15
	printf -v _TMP "%2.2f" ${T_CPU/\./\,}
	print_right $_TMP 9
	printf -v _TMP "%2.2f" ${T_NOTA/\./\,}
	print_right $_TMP 10
	echo
	echo "|______________________________________________________________"
	echo "|--------------------------------------------------------------"

	#
	# Calcular e imprimir nota global, incluyendo penalizaciÃ³n por warnings
	#
	if [[ "$WARN" > "0" ]]; then
		P_WARNS=$(echo "scale=3; $WARN * $POR_WARNING" | bc)
		T_NOTA=$(echo "scale=3; $T_NOTA - $P_WARNS" | bc)
		printf "| PENALIZACION POR %d WARNING(S): %1.3f\n" $WARN ${P_WARNS/\./\,}
	fi
	if [ $(echo "scale=3; $T_NOTA < 0.0" | bc) = 1 ]; then
		T_NOTA=0
	fi
	printf "| NOTA DEFINITIVA DE CORRECCION: %.3f\n" ${T_NOTA/\./\,}
	echo "|______________________________________________________________"

	# Hoja de resultados finales
	print_final_results "ok" "$T_NOTA" "${N_EJECS[$FASE]}" "$T_MOVS" "$T_INFS" "$T_CPU" > "$RESULTS_FILE"
}

#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------
# Funciones auxiliares
#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------

##
## Crear un email para enviar al alumno, imprimiéndolo por la salida standard
##
create_email()
{
	echo "<html><body>"
	echo "<p>Hola $ALUMNO:</p>"
	echo "<p>  "
	echo " A continuación se detallan los resultados de la corrección automática de la "
	echo " última solución que has entragado para tu mapa numero $MAPA_ALU de la fase $FASE. "
	echo " <br>"
	echo " En este email sólo encontrarás el resumen de la corrección. Para consultar los "
	echo " detalles sobre cada ejecución que hay hecho el servidor de corrección, así como "
	echo " las notas particulares de cada una de estas correcciones o los warnings y errores "
	echo " de compilación, dirígete al servidor de entrega. Entrando con tu usuario tendrás "
	echo " acceso a toda la información sobre tu corrección. "
	echo "</p><p>"
	echo " Si consideras que puede haber algún error con tu corrección o tienes cualquier tipo "
	echo " de problemas, ponte en contacto con tu profesor de prácticas lo antes posible."
	echo "</p><p>"
	echo " Un saludo,"
	echo " <br>"
	echo " Script autocorrector 1.0."
	echo "</p><p>"
	echo "*************************************************************************************"
	echo "</p>"
	echo "<code>"
	sed s/$/\<br\>/ $EXE.$DSUM | sed "s/\ /\&nbsp\;/g"
	echo "</code>"
	echo "</body></html>"
}


##
## Eliminar las rutas que preceden a los nombres de archivo entregados por alumnos
##  $1 = Ruta al fichero temporal con los mensajes de error de compilaciÃ³n
##  $2 = Nombre de fichero entregado por el alumno (SIN RUTA ANTERIOR, SOLO NOMBRE DE ARCHIVO!)
##
remove_paths_from_error_messages()
{
	local DTMP2=totaltemp2
	local __FILE="${2//\./\\.}"
	sed "s/\/.*$__FILE/$__FILE/" "$1" > "$1.$DTMP2"
	mv "$1.$DTMP2" "$1"
}

##
## Escribe los resultados finales en un formato fÃ¡cil para ser interpretado por el padre
##   $1 = RESULTADO DE EJECUCION (ok, error)
##   $2 = NOTA FINAL
##   $3 = NUMERO DE EJECUCIONES REALIZADAS
##   $4 = MEDIA DE MOVIMIENTOS
##   $5 = MEDIA DE INFERENCIAS
##   $6 = MEDIA DE TIEMPO DE CPU
## 
print_final_results()
{
	echo "RESULTADO DE COMPILACION (ok, no_compila)"
	echo "$1"
	echo "NOTA"
	printf "%.3f\n" ${2/\./\,}
	echo "NUM. EJECUCIONES"
	echo "$3"
	echo "MEDIA DE MOVIMIENTOS"
	printf "%.3f\n" ${4/\./\,}
	echo "MEDIA DE INFERENCIAS"
	printf "%.3f\n" ${5/\./\,}
	echo "MEDIA DE TIEMPOS DE CPU"
	printf "%.3f\n" ${6/\./\,}
}

##
## Ejecutar una aplicaciÃ³n con un timeout en caso de que se quede colgada
##   $1 = Comando
##   $2 = Timeout
##   $3 = Archivo de salida (redirigido con &>)
##
execute_with_timeout()
{
	# Ejecutamos con o sin redirecciÃ³n, segÃºn parametros
	local __REDIR="$3"
	if [ ${__REDIR:=""} == "" ]; then
		( $1 ) &
	else
		( $1 &> $__REDIR ) &
	fi
	export _SON_PID=$!
	export _OWN_PID=$$
	export _TIMEOUT=$2
	
	# SubfunciÃ³n para atrapar la seÃ±al de timeout y matar!
	timeout_kill()
	{
		# Si aÃºn estÃ¡ viva la aplicaciÃ³n, nos la cargamos
		ps -p $_SON_PID | grep -v "PID.*CMD" &> /dev/null
		if [ $? == 0 ]; then
			kill -KILL $_SON_PID &> /dev/null
		fi
	}

	# Atrapamos la seÃ±al para el caso de que haya timeout
	trap timeout_kill SIGUSR1

	# Lanzamos el contador de timeout y nos quedamos con si PID de proceso
	(sleep $_TIMEOUT; kill -SIGUSR1 $_OWN_PID) &
	local _TIMER_PID=$!

	# Esperamos a que termine nuestro hijo y matamos al hijo cuando lo haga
	wait $_SON_PID
	
	# Comprobamos si el hijo ha salido por motu propio o muerte prematura :)
	local _RETORNO=$?
	if [ $_RETORNO != 138 ]; then
		kill $_TIMER_PID &> /dev/null
	fi

	# Devolvemos lo que la aplicaciÃ³n nos haya dado, o 138 si la hemos matado
	return $_RETORNO
}

##
## Calcula la nota final que se debe obtener de una ejecucion a partir 
## de un archivo con el resultado (sin errores)
##
calculate_execution_mark()
{
	# Obtener los valores del archivo de resultados $1
	#
	egrep -i "limite.*superado" $1 &> /dev/null
	local _LIMITE=$((!$?))
	local _NOTA=`egrep -i "[0-9,\.]+\%" -o $1 | egrep -o "[0-9,\.]+"`
	local _MOVIMIENTOS=`egrep -i "movimientos" $1 | egrep -o "[0-9]+ "`
	local _INFERENCIAS=`egrep -i "inferencias" $1 | egrep -o "[0-9]+"`
	local _TIEMPO_CPU=`egrep -i "tiempo" $1 | egrep -o "[0-9,\.]+"`
	local _COLISIONES=`egrep -i "colisiones" $1 | egrep -o "[0-9]+"`
	local _INTENTOS=`egrep -i "intentos" $1 | egrep -o "[0-9]+"`
	local _ACCIONES_ERR=`egrep -i "erroneas" $1 | egrep -o "[0-9]+"`
	local _FALLOS=`egrep -i "fallos" $1 | egrep -o "[0-9]+"`

	# Calcular nota
	#
	local _NOTA_EJECUCION=0
	_NOTA_EJECUCION=$(echo "scale=3; ($_NOTA/10.0 - $_COLISIONES*$POR_COLISION - $_INTENTOS*$POR_INTENTO - $_LIMITE*$POR_MAX_MOVIMIENTOS)" | bc)
	_NOTA_EJECUCION=$(echo "scale=3; ($_NOTA_EJECUCION - $_ACCIONES_ERR*$POR_ACCION_ERRONEA - $_FALLOS*$POR_FALLO_DE_REGLA)" | bc)
	if [ $(echo "scale=3; $_NOTA_EJECUCION < 0.0" | bc) = 1 ]; then
		_NOTA_EJECUCION=0
	fi

	# Asignar valores a los arrays globales
	#
	NOTA[$2]=$_NOTA_EJECUCION
	MOVIMIENTOS[$2]=$_MOVIMIENTOS
	INFERENCIAS[$2]=$_INFERENCIAS
	TIEMPOCPU[$2]=$_TIEMPO_CPU
	
	# Imprimir resultados
	#
	print_right "$_MOVIMIENTOS" 15
	print_right "$_INFERENCIAS" 14
	printf -v _TMP "%2.2f" ${_TIEMPO_CPU/\./\,}
	print_right "$_TMP" 9
	printf -v _TMP "%2.2f" ${_NOTA_EJECUCION/\./\,}
	print_right "$_TMP" 10
	echo
	
	# AÃ±adir cÃ¡lculo de resultaods al archivo de resultados
	#
	local _ALIGN_RIGHT=8
	echo >> $1
	echo    "=============================================================" >> $1
	echo    "||          CALCULO DE NOTA RESULTANTE" >> $1
	echo    "|------------------------------------------------------------" >> $1
	echo -n    "| Nota (por cocos comidos):              " >> $1
	__TMP=$(echo "scale=3; ($_NOTA/10.0)" | bc)
	printf -v _TMP "%2.3f" ${__TMP/\./\,}
	print_right $_TMP $_ALIGN_RIGHT >> $1
	echo >> $1
	print_final_results_added_field "| Pena por colisiones:                   " $_ALIGN_RIGHT $_COLISIONES   $POR_COLISION        $1
	print_final_results_added_field "| Pena por intentos de accion:           " $_ALIGN_RIGHT $_INTENTOS     $POR_INTENTO         $1
	print_final_results_added_field "| Pena por acciones erroneas:            " $_ALIGN_RIGHT $_ACCIONES_ERR $POR_ACCION_ERRONEA  $1
	print_final_results_added_field "| Pena por fallos de la regla:           " $_ALIGN_RIGHT $_FALLOS       $POR_FALLO_DE_REGLA  $1
	print_final_results_added_field "| Pena por exceder limite de movimientos:" $_ALIGN_RIGHT $_LIMITE       $POR_MAX_MOVIMIENTOS $1
	echo    "|------------------------------------------------------------" >> $1
	echo -n    "| NOTA FINAL(descontadas penalizaciones):" >> $1
	printf -v _TMP "%2.3f" ${_NOTA_EJECUCION/\./\,}
	print_right $_TMP $_ALIGN_RIGHT >> $1
	echo >> $1
	echo    "=============================================================" >> $1
}

##
## Imprimir un campo de nota aÃ±adido al archivo de resultados
##  $1 = TEXTO
##  $2 = ALINEACION
##  $3 = VALOR DEL CAMPO
##  $4 = PENALIZACION ASOCIADA
##  $5 = ARCHIVO DE SALIDA
##
print_final_results_added_field()
{
	if (( $3 > 0 )); then
		echo -n "$1" >> $5
		local __TMP=$(echo "scale=3; ($3*$4)" | bc)
		printf -v _TMP "%2.3f" ${__TMP/\./\,}
		print_right "-$_TMP" $2 >> $5
		echo >> $5
	fi
}

##
## Imprimir cadena alineada a la derecha
##  $1 cadena a imprimir
##  $2 tamaÃ±o del campo en pantalla
##
print_right()
{
	CAD="$1"
	SCR_LEN=$2	

	# Si la longitud de la cadena es mayor que el tamaÃ±o en pantalla, imprimir sin mÃ¡s
	# Si no, ponemos los espacios necesarios de relleno
	# 
	if (( ${#CAD} > $SCR_LEN )); then
		echo -n "$CAD"
		return
	else
		DIF=$(($SCR_LEN - ${#CAD}))
		for _KK in $(seq 1 $DIF);do echo -n " "; done
		echo -n "$CAD"
	fi
}

##
## Crear una carpeta cualquiera en caso de que no exista
##
create_folder()
{
	if [ ! -e $1 ]; then
		mkdir $1
	elif [ ! -d $1 ]; then
		echo "ERROR: $1 ya existe y no es un directorio"
		exit 1
	elif [ ! -w $1 ]; then
		echo "ERROR: Sin permiso de escritura en $1"
		exit 2
	fi
}


##
## EXPLICACIÃ“N DE USO POR PANTALLA
##
function usage()
{
	echo "Uso:"
	echo
	echo "$0 ALU FA MA_R MA_A SOL RE [PARAMETROS]"
	echo
	echo "Script autocorrector de practicas. Autocorrige una solucion aportada por un"
	echo "para una fase y un mapa concretos, otorgandole una nota objetiva. "
	echo "Para poder realizar la correccion necesita conocer el acronimo del alumno(ALU)"
	echo "el numero de fase (FA), el numero del mapa a corregir (MA_R) el numero de mapa"
	echo "que tiene asignado el alumno en esta fase (MA_A), el archivo de solucion"
	echo "que ha aportado el alumno (SOL) y el nombre de la regla del alumno (RE)."
	echo
	echo "El comportamiento por defecto se puede cambiar con los parametros opcionales."
	echo
	echo "PARAMETROS:"
	echo "  -e NE       --executions NE"
	echo "     Establece el numero de veces que se va a ejecutar pl-man con esta solucion"
	echo "    para poder hacer una evaluacion estadistica."
	echo 
	echo "  -h          --help"
	echo "     Muestra esta pantalla de ayuda"
	echo
	echo "  -l \"EM\"       --email-to \"EM\""
	echo "     Envia un email a la direccion de correo EM con los resultados globales de la "
	echo "    correccion realizada. OJO! La direccion de correo debe ir entre comillas dobles."
	echo 
	echo "  -m MF       --main-folder MF"
	echo "     Cambia la ruta por defecto de la raiz del arbol de directorios donde se "
	echo "    guardan todos los archivos de resultados y logs de ejecuciones. "
	echo 
	echo "  -M MF   --map-file MF"
	echo "     Utiliza el fichero de mapa MF en lugar del fichero propio de la fase y mapa."
	echo "    MF se entiende que puede ser tanto un nombre de fichero como una ruta."
	echo
	echo "  -n \"N\"    --student-name \"N\""
	echo "     Establece el nombre completo del estudiante que aporta la solucion. "
	echo "    OJO! El nombre del estudiante debe ir entre comillas"
	echo
	echo "  -r RF       --results-file RF"
	echo "     Modifica el nombre del archivo de resultados por defecto. Este es el archivo"
	echo "    donde se escribiran los resultados principales de la correccion en un formato"
	echo "    facilmente parseable por un programa padre que quiera obtenerlos."
	echo
	echo "  -t TO       --timeout TO"
	echo "     Establece los segundos maximos que debe durar una ejecucion individual de "
	echo "    la solucion al mapa. Cuando este tiempo es sobrepasado, se considera que la "
	echo "    ejecucion esta fuera de tiempo (TIMEOUT), se mata al proceso de pl-man, se "
	echo "    notifica al alumn@ y se pone un 0 de calificacion a esa ejecucion, asumiendo "
	echo "    que debe haber entrado en un bucle infinito. "
	echo
	echo "  -v MM       --max-movements MM"
	echo "     Establece el numero maximo de movimientos permitidos para el mapa que va"
	echo "    a ser corregido. "
	echo 
	exit
}

##
## PROCESADO DE PARAMETROS
##
function processparameters()
{
	MAIN_PARAMS=0			# NÃºmero de parÃ¡metros principales procesados
	EXECUTIONS=-1			# NÃºmero de ejecuciones dado por el usuario
	PREV_TIMEOUT=-1		# Segundos mÃ¡ximos de espera para cada ejecuciÃ³n (timeout)
	MAX_MOVEMENTS=-1     # Número máximo de movimientos para resolver el mapa

	# Comprobamos nÃºmero de parÃ¡metros (incluyendo nombre del script)
	if (( $# < 6 )); then 
		usage $0
	fi

	# Procesamos parÃ¡metros opcionales
	while (( $# ))
	do
		case $1 in
			-e|--executions)
				# Comprobar que se da un valor entero de numero de ejecuciones
				if [[ ! "$2" =~ (^[0-9]+$) ]]; then
					echo "ERROR: Numero de ejecuciones ($2) incorrecto "
					echo "   -e y --executions deben ir seguidos de un numero entero"
					echo "   que represente el numero de ejecuciones a realizar."
					exit 21
				fi
				EXECUTIONS=$2
				shift
			;;
			-h|--help)
				usage
			;;
			-l|--email-to)
				# Comprobar que se da una dirección de email válida
				if [[ ! "$2" =~ (^[^@\ ]+@([^@\ \.]+\.)+[^@\ \.]+$) ]]; then
					echo "ERROR: ($2) no es una direccion de correo valida. "
					echo "   -l y --email-to deben ir seguidos de la direccion de correo del alumno."
					exit 28
				fi
				EMAIL_ADDRESS="$2"
				shift
			;;
			-m|--main-folder)
				# Comprobar que se da un nombre de carpeta
				if [[ ! "$2" =~ (^[^\-].+$) ]]; then
					echo "ERROR: -m y --main-folder deben ir seguidos de una ruta valida que"
					echo "   represente el nodo raiz del arbol de directorios donde guardar los datos."
					exit 22
				fi
				MAIN_FOLDER=$2
				shift
			;;
			-M|--map-file)
				# Comprobar que se aporta una ruta al fichero del mapa
				if [[ ! "$2" =~ (^[^\-].+$) ]]; then
					echo "ERROR: Se esperaba una ruta o nombre de fichero de mapa tras"
					echo "       -M o --map-file, se recibio ($2)"
					exit 27
				fi
				ALTERNATE_MAP_FILE="-M $2"
				shift
			;;
			-n|--student-name)
				# Comprobar que se da un nombre de estudiante
				if [[ ! "$2" =~ (^[^\-].+$) ]]; then
					echo "ERROR: -n y --student-name deben ir seguidos de un nombre de estudiante valido, "
					echo "   generalmente entre comillas dobles."
					exit 23
				fi
				ALUMNO="$2"
				shift
			;;			
			-r|--results-file)
				# Comprobar que se da un nombre de archivo
				if [[ ! "$2" =~ (^[^\-].+$) ]]; then
					echo "ERROR: -r y --result-file deben ir seguidos de un nombre de archivo valido."
					exit 26
				fi
				RESULTS_FILE="$2"
				shift
			;;			
			-v|--max-movements)
				# Comprobar que se da un valor entero de numero de maximos movimientos
				if [[ ! "$2" =~ (^[0-9]+$) ]]; then
					echo "ERROR: Numero de maximos movimientos ($2) incorrecto "
					echo "   -v y --max-movements deben ir seguidos de un numero entero"
					echo "   que represente el numero maximo de movimientos permitidos."
					exit 24
				fi
				MAX_MOVEMENTS=$2
				shift
			;;
			-t|--timeout)
				# Comprobar que se da un valor flotante de seguntos de timeout
				if [[ ! "$2" =~ (^[0-9]+$)|(^[0-9]+\.[0-9]+$)|(^\.[0-9]+$) ]]; then
					echo "ERROR: Valor de segundos de timeout ($2) incorrecto "
					echo "   -t y --timeout deben ir seguidos de un numero real que represente"
					echo "   los segundos maximos de espera de resultado para una ejecucion."
					exit 25
				fi
				PREV_TIMEOUT=$2
				shift
			;;
			*)
				case $MAIN_PARAMS in
					0)
						# Primer parámetro: ACRONIMO DEL ALUMNO (no vacio)
						if [[ ! "$1" =~ (^[a-zA-Z].*$) ]]; then
							echo "ERROR: El acronimo del alumno ($1) debe comenzar por un caracter alfabetico"
							echo
							exit 10
						fi
						ALU_ACRO=$1
					;;
					1)
						# Segundo parÃ¡metro: NUMERO DE FASE, entre 0 y 6
						if [[ ! "$1" =~ (^[0-6]$) ]]; then
							echo "ERROR: El numero de fase ($1) debe estar entre 0 y 6"
							echo
							exit 11
						fi
						FASE=$1
					;;
					2)
						# Tercer parÃ¡metro: MAPA REAL, entre 00 y 50
						if [[ ! "$1" =~ (^[0-5][0-9]$) ]]; then
							echo "ERROR: El numero de mapa REAL ($1) debe estar entre 00 y 50 (2 digitos)"
							echo
							exit 12
						fi
						MAPA_REAL=$1
					;;
					3)
						# Cuarto parÃ¡metro: MAPA DEL ALUMNO, entre 0 y 9
						if [[ ! "$1" =~ (^[0-9]$) ]]; then
							echo "ERROR: El numero de mapa del ALUMNO ($1) debe estar entre 0 y 9 (1 digito)"
							echo
							exit 13
						fi
						MAPA_ALU=$1
					;;
					4)
						# Quito parÃ¡metro: FICHERO DE SOLUCION (que debe existir y ser legible)
						if [ -f "$1" -a -r "$1" ]; then
							SOL_FILE=$1
						else
							echo "ERROR: El fichero de solucion ($1) no es correcto, no existe o no tiene permiso de lectura"
							echo
							exit 14
						fi
					;;
					5)
						# Sexto parÃ¡metro: NOMBRE DE LA REGLA
						RULE=$1
					;;
					*)
						echo "ERROR: Parametro no identificado ($1)"
						exit 70
					;;
				esac
				MAIN_PARAMS=$(($MAIN_PARAMS + 1))
			;;
		esac
		shift
	done
	
	# Post-procesado de parametros
	if [ "$EXECUTIONS" != -1 ]; then
		N_EJECS[$FASE]=$EXECUTIONS
	fi
	if [ "$PREV_TIMEOUT" != -1 ]; then
		T_TIMEOUT[$FASE]=$PREV_TIMEOUT
	fi
	if [ "$MAX_MOVEMENTS" == "-1" ]; then
		MAX_MOVEMENTS=${N_MAXMOVS[$FASE]}
	fi
}

#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------
main "$@" 2> /dev/null
exit 0
