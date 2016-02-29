#!/bin/bash

# Variables
#
SWIPL=swipl						# Nombre del ejecutable de swi-prolog
MAPS_DIR=maps/2008/fase_		                # Directorio donde encontrar los mapas
MAP_FILE=mapa.pl					# Fichero de mapa (ruta completa)
PHASE=0							# Número de fase
MAP=00							# Número de mapa
SOL_FILE=solucion.pl					# Archivo con las reglas para resolver el mapa
DELAY=keypress						# Delay de ejecución del mapa
RULE=true						# Por defecto no hay regla de ejecucion
L_PARAMS="0"						# Por defecto no hay parámetros de ejecución
COMP_FILE=compilation_log.pl.temp			# Archivo temporal para guardar la compilación
OUT_FILE=a.out						# Archivo de salida
EXECUTE_BIN=1						# Ejecutar archivo binario tras compilar?
DELETE_BIN=1						# Borrar archivo binario tras ejecutar?
MODE=play						# Modo de ejecución (play/replay)
MAIN_FILE=pl-man-game/main.pl				# Archivo principal de ejecución de pl-man
##
## Función principal del script
##
function main()
{
	# Primero, procesar parámetros de entrada
	processparameters $@

	# Compilar
	echo "Compilando..."
	if [ "$MODE" = "play" ]; then
		$SWIPL -O -g "play('$MAP_FILE', $RULE, $DELAY, [ eval(true), $L_PARAMS ]), halt" -o $OUT_FILE -c $SOL_FILE 2> $COMP_FILE
	else
		$SWIPL -O -g "replay('$SOL_FILE', []), halt" -o $OUT_FILE -c $MAIN_FILE 2> $COMP_FILE		
	fi

	# Comprobar si la compilación ha tenido éxito
	if [ "`grep -e "ERROR" $COMP_FILE`" != "" ]; then
		echo "*************************************************"
		echo "*********** ERROR DE COMPILACION ****************"
		echo "*************************************************"
		echo
		cat $COMP_FILE
		exit 10
	elif [ "`grep -e "Warning" $COMP_FILE`" != "" ]; then
		echo "/////// WARNINGS COMPILANDO \\\\\\\\\\\\\\\\"
		echo "-------------------------------------"
		echo
		cat $COMP_FILE
	else
		echo "Archivos compilados con éxito :)"
		rm $COMP_FILE
		if (( $EXECUTE_BIN )); then
			echo "Ejecutando..."
			./$OUT_FILE
		fi
		if (( $DELETE_BIN )); then
			rm $OUT_FILE
		fi
	fi
	
	# Todo correcto
	exit 0
}

##
## EXPLICACIÓN DE USO POR PANTALLA
##
function usage()
{
	echo "Uso:"
	echo
	echo "$0 FA MA RE [PARAMETROS]"
	echo "$0 -r logfile"
	echo
	echo "Compila el mapa numero MA de la fase FA para que sea resuelto por "
	echo "la regla RE que se debe encontrar en el archivo de solucion. Por defecto,"
	echo "lanza la ejecución para trazado paso a paso (keypress), con visualizacion"
	echo "activada y asumiendo que el archivo de solucion sera solucion.pl y que "
	echo "estara en el mismo directorio que el mapa. " 
	echo 
	echo "Tambien permite reproducir un log de una ejecucion anterior con la opcion -r."
	echo "Esta opcion modifica el procesado de parametros, haciendo innecesarios los "
	echo "3 parametros principales FA,MA y RE, ya que para reproducir un log no son "
	echo "necesarios."
	echo
	echo "El comportamiento por defecto se puede cambiar con los parametros opcionales."
	echo
	echo "PARAMETROS:"
	echo "  -d S    --delay S"
	echo "    Cambia el modo de ejecución paso a paso por un modo de ejecución continua"
	echo "    con S segundos de espera entre movimientos."
	echo 
	echo "  -h      --help"
	echo "    Muestra esta pantalla de ayuda. "
	echo
	echo "  -l LF   --log-file LF"
	echo "    Indica a pl-man que cree un log de la ejecución y lo guarde en el archivo"
	echo "    LF para poder reproducirlo en un futuro si es necesario. "
	echo
	echo "  -m MV   --max-movements MV"
	echo "    Ejecuta pl-man restringiendo la cantidad de movimientos disponibles para"
	echo "    resolver el mapa a MV movimientos como máximo."
	echo
	echo "  -M MF   --map-file MF"
	echo "     Utiliza el fichero de mapa MF en lugar del fichero propio de la fase y mapa."
	echo "    MF se entiende que puede ser tanto un nombre de fichero como una ruta."
	echo
	echo "  -n      --no-draw"
	echo "    Ejecuta pl-man sin visualizacion. Solo imprime por pantalla los mensajes"
	echo "    de compilacion y las estadisticas finales al terminar."
	echo
	echo "  -o OF   --output OF"
	echo "    Por defecto, una vez termina la compilacion de forma exitosa se ejecuta"
	echo "    el archivo binario y despues se elimina. El parametro -o --output compila"
	echo "    y genera un archivo binario de salida OF que no es ejecutado ni eliminado."
	echo
	echo "  -r RF   --replay RF"
	echo "	  Activa el modo de reproduccion de un log de ejecucion anterior. El     "
	echo "    reproductor de logs no requiere los parámetros de fase, mapa ni regla; "
	echo "    solo se limita a leer el log de ejecucion anterior y reproducirlo.     " 
	echo
	echo "  -s SF   --solution-file SF"
	echo "	  En lugar de buscar la regla en el archivo solucion.pl la busca en el"
	echo "    archivo SF. SF puede ser el nombre del archivo local o su ruta."
	echo
	echo "  -t TF   --temporal-file TF"
	echo "	  Especifica el nombre del archivo temporal donde se almacenarán los resultados"
	echo "    de la compilación. " 
	echo
	exit
}


#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------
# Funciones auxiliares
#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------

##
## PROCESADO DE PARAMETROS
##
function processparameters()
{
	MAIN_PARAMS=0					# Número de parámetros principales procesados
	PROV_OUT_FILE=kkkjjjkkklll123		        # Archivo de salida provisional
	_PROV_MAP_FILE=""				# Ruta a fichero de mapa alternativo provisional
	CHECK_SOL_FILE=$SOL_FILE	# Para comprobar si el archivo de solución es modificado o no

	# Comprobamos número de parámetros (incluyendo nombre del script)
	if (( $# < 1 )); then 
		usage $0
	fi

	# Procesamos parámetros opcionales
	while (( $# ))
	do
		case $1 in
			-d|--delay)
				# Comprobar que el Delay es un número entero o real válido
				if [[ ! "$2" =~ (^[0-9]+$)|(^[0-9]+\.[0-9]+$)|(^\.[0-9]+$) ]]; then
					echo "ERROR: delay incorrecto"
					echo "   -d y --delay deben ir seguidos de un numero entero o real"
					echo "   representando los segundos de espera entre movimientos."
					exit 3
				fi
				DELAY=$2
				shift
			;;
			-m|--max-movements)
				# Comprobar que se aporta un parámetro numérico entero
				if [[ ! "$2" =~ (^[0-9]+$) ]]; then
					echo "ERROR: El número máximo de movimientos (-m --max-movements) debe se entero"
					exit 9
				fi
				L_PARAMS="$L_PARAMS, max_moves($2)"
				shift
			;;
			-M|--map-file)
				# Comprobar que se aporta una ruta al fichero del mapa
				if [[ ! "$2" =~ (^[^\-].+$) ]]; then
					echo "ERROR: Se esperaba una ruta o nombre de fichero de mapa tras"
					echo "       -M o --map-file, se recibio ($2)"
					exit 9
				fi
				_PROV_MAP_FILE="$2"
				shift
			;;
			-t|--temporal-file)
				# Comprobar que se aporta un parámetro no vacio como nombre de fichero
				if [[ ! "$2" =~ (^.+$) ]]; then
					echo "ERROR: nombre de fichero temporal de compilación requerido tras -t o --temporal-file"
					exit 11
				fi
				COMP_FILE=$2
				shift
			;;
			-s|--solution-file)
				# Comprobar que se aporta un parámetro no vacio como nombre de fichero
				if [[ ! "$2" =~ (^.+$) ]]; then
					echo "ERROR: nombre de fichero de solucion requerido tras -s o --solution-file"
					exit 4
				fi
				SOL_FILE=$2
				shift
			;;
			-r|--replay)
				# Comprobar que se aporta un parámetro no vacio como nombre de fichero de log
				if [[ ! "$2" =~ (^.+$) ]]; then
					echo "ERROR: nombre de fichero de log requerido tras -r o --replay"
					exit 4
				fi
				SOL_FILE=$2
				MODE=replay
				shift
			;;
			-l|--log-file)
				# Comprobar que se aporta un parámetro no vacio como nombre de fichero de log
				if [[ ! "$2" =~ (^.+$) ]]; then
					echo "ERROR: nombre de fichero de solucion requerido tras -l o --log-file"
					exit 5
				fi
				L_PARAMS="$L_PARAMS, log_file('$2')"
				shift
			;;
			-o|--output)
				# Comprobar que se aporta un parámetro no vacio como nombre de archivo binario
				if [[ ! "$2" =~ (^.+$) ]]; then
					echo "ERROR: nombre de archivo binario de salida requerido tras -o o --output"
					exit 6
				fi
				PROV_OUT_FILE=$2
				EXECUTE_BIN=0
				DELETE_BIN=0
				shift
			;;
			-n|--no-draw)
				L_PARAMS="$L_PARAMS, drawing(false)"
				DELAY=0
			;;
			-h|--help)
				usage
			;;
			*)
				case $MAIN_PARAMS in
					0)
						# Primer parámetro: Número de fase, entre 0 y 9
						if [[ ! "$1" =~ (^[0-9]$) ]]; then
							echo "ERROR: El numero de fase ($1) debe estar entre 0 y 9"
							echo
							exit 1
						fi
						PHASE=$1
					;;
					1)
						# Segundo parámetro: Número de mapa de 00 a 99
						if [[ ! "$1" =~ (^[0-9]{2}$) ]]; then
							echo "ERROR: El numero de mapa ($1) debe ser un entero entre 00 a 99 (2 digitos)"
							echo
							exit 2
						fi
						MAP=$1
					;;
					2)
						# Tercer parámetro: Nombre de la regla (y de archivo de salida por defecto)
						RULE=$1
						OUT_FILE=$RULE.pl-man
					;;
					*)
						echo "ERROR: Parametro no identificado ($1)"
						exit 7
					;;
				esac
				MAIN_PARAMS=$(($MAIN_PARAMS + 1))
			;;
		esac
		shift
	done

	# Sólo continuamos si el modo no es el de repetición
	if [ "$MODE" = "play" ]; then		
		# Comprobar si están todos los parámetros requeridos
		if [ "$MAIN_PARAMS" != "3" ]; then
			case $MAIN_PARAMS in
				0) echo "ERROR: Faltan los 3 parámetros principales (fase FA, mapa MA y regla RE)" ;;
				1) echo "ERROR: Faltan los 2 parámetros principales (mapa MA y regla RE)" ;;
				2) echo "ERROR: Falta el nombre de la regla RE a ejecutar." ;;
			esac
			exit 9
		fi

		# Ajustar parámetros
		MAPS_DIR=$MAPS_DIR$PHASE/$MAP
		if [ "$_PROV_MAP_FILE" != "" ]; then
			MAP_FILE=$_PROV_MAP_FILE
		else
			MAP_FILE=$MAPS_DIR/mapa$PHASE-$MAP.pl
		fi
		if [ "$CHECK_SOL_FILE" = "$SOL_FILE" ]; then
			SOL_FILE=$MAPS_DIR/$SOL_FILE
		fi
		MODE=play
	fi

	# Archivo de salida
	if [ "$PROV_OUT_FILE" != "kkkjjjkkklll123" ]; then
		OUT_FILE=$PROV_OUT_FILE
	fi
}

#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------
main $@
exit 0
