#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>
#include <sstream>

using namespace std;
const char* MAP_FORMAT_VERSION = "1.0";


// Estructura para posiciones
struct TPos
{
	TPos(int _x, int _y): x(_x), y(_y) {};
	int x, y;
};

//
// Escribir la forma en que se usa este programa por pantalla y terminar
//
void
uso()
{
	cerr << "\nconversion <archivo.txt>\n\n";
	cerr << "Convierte el mapa que contiene archivo.txt en un archivo de codigo de prolog\n";
	cerr << "preparado para ser leido por el juego pl-man.\n\n";
	cerr << "El mapa definido en archivo.txt puede contener los siguientes elementos:\n";
	cerr << " - Cocos:    '.'    (Debe haber almenos 1)\n";
	cerr << " - Pacman:   '@'    (Debe haber 1)\n";
	cerr << " - Objetos:  'O'    (De 0 a N)\n";
	cerr << " - Enemigos: 'E'    (De 0 a N)\n";
	cerr << " - Espacios: ' '    (De 0 a N)\n";
	cerr << " - Cualquier otro caracter sera tratado como una pared.\n\n";
	cerr << "Es obligatoria la presencia de una unica posicion de salida para el pacman,\n";
	cerr << "asi como la presencia de almenos 1 coco. El mapa debe ser perfectamente rec-\n";
	cerr << "tangular y el archivo no debe contener saltos de linea al final. Los  carac-\n";
	cerr << "teres 'O' y 'E' marcaran  lugares de salida de  objetos y enemigos, pero no \n";
	cerr << "definiran ni el tipo ni el comportamiento de esos  objetos y enemigos. Para \n";
	cerr << "esto ultimo, es responsabilidad tuya retocar el archivo .pl de salida.\n\n";

	cerr << (int)'\n';

	exit(1);
}

//
// Escribir el contenido de una nueva celda del mapa en el código pl y actualizar X
//
void
writeNewCell(char content, int& xpos)
{
	if (xpos > 0) cout << ", ";
	cout << "\'";
	if (content == '\\') cout << "\\";
	cout << content << "\'";
	xpos++;
}

//
// Programa principal (El parser)
//
int
main (int argc, char **argv)
{
	vector<char> vecSolids;
	vector<TPos> vecObjects;
	vector<TPos> vecEnemies;
	char sigchar;
	int ncocos = 0, ty = 0, tx1 = -1, tx2 = 0;
	int pacman_x = -1, pacman_y = -1;

	if (argc != 2)
		uso();

	// Abrir el archivo con la definición del mapa para lectura
	//
	ifstream fentrada(argv[1]);
	if (!fentrada.is_open())
	{
		cerr << "Error: No se pudo abrir el archivo " << argv[1] << ".\n";
		exit(-1);
	}

	// Comenzamos a generar el código prolog por la salida estándar
	cout << "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n";
	cout << "%%% Prolog Code for Map-file generated from " << argv[1] << endl;
	cout << "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n\n";
	cout << "map_format_version("<< MAP_FORMAT_VERSION <<").\n";
	cout << "map([[";

	// Vamos leyendo caracteres y transformándolos
	//
	while(!fentrada.eof())
	{
		sigchar = fentrada.get();

		switch(sigchar)
		{
			case '\n':	/// SALTO DE LÍNEA
			{
				cout << "],\n[";
				if      (tx1 == -1) tx1 = tx2;
				else if (tx1 != tx2)
				{
					cerr << "\nError en el mapa\n";
					cerr << "   Hay filas de distinta longitud. El mapa debe ser cuadrado.\n";
					exit(2);
				}
				ty++;
				tx2 = 0;
				break;
			}
			case ' ':
			case '.':	/// COCO Y ESPACIO
			{
				if (sigchar == '.') ncocos++;
				writeNewCell(sigchar, tx2);
				break;
			}
			case '@':	/// PACMAN
			{
				if (pacman_x != -1 || pacman_y != -1)
				{
					cerr << "\nError en el mapa\n";
					cerr << " Se encontró más de una posición inicial para pacman\n";
					exit(6);
				}
				else
				{
					pacman_x = tx2;
					pacman_y = ty;
					writeNewCell(' ', tx2);
				}
				break;
			}
			case 'O':	/// OBJETO
			{
				vecObjects.push_back(TPos(tx2, ty));
				writeNewCell(' ', tx2);
				break;
			}
			case 'E':	/// ENEMIGO
			{
				vecEnemies.push_back(TPos(tx2, ty));
				writeNewCell(' ', tx2);
				break;
			}
			case EOF:	/// FIN DE FICHERO
				break;
			default: 	/// NO PERMITIDO O PARED
			{
				if (sigchar < 32)
				{
					cerr << "\nError en el mapa\n";
					cerr << " Se encontró un caracter no permitido (" << sigchar << ", " << (int)sigchar << ")\n";
					exit(3);
				}
				else
				{
					// Si este sólido no estaba catalogado, lo hacemos ahora
					vector<char>::iterator it;
					it = find(vecSolids.begin(), vecSolids.end(), sigchar);
					if (it == vecSolids.end())
						vecSolids.push_back(sigchar);
					// Y escribimos la celda en la salida estándar
					writeNewCell(sigchar, tx2);
				}
			}
		}
	}
	cout << "]]).\n";

	// Comprobar posibles errores a posteriori
	//
	if (pacman_x < 0 || pacman_y < 0)
	{
		cerr << "\nError en el mapa\n";
		cerr << " No se ha especificado posición inicial para el pacman.\n";
		exit(4);
	}
	if (ncocos < 1)
	{
		cerr << "\nError en el mapa\n";
		cerr << " El mapa expuesto no contiene ningún coco.\n";
		exit(5);
	}

	// Escribir predicados finales
	//
	cout << "map_size(" << tx1 << ", " << ty+1 << ").\n";
	cout << "num_dots(" << ncocos << ").\n";
	cout << "pacman_start(" << pacman_x << ", " << pacman_y << ").\n";
	cout << "initMap";

	stringstream ssInitMapContent;
	ssInitMapContent.str("");

	// Solidos
	unsigned i, j, k;
	for(i = 0; i < vecSolids.size(); i++)
	{
		if(i > 0) ssInitMapContent << ", \n";
		ssInitMapContent << "\taddSolidObject(\'";
		if (vecSolids[i] == '\\') ssInitMapContent << '\\';
		ssInitMapContent << vecSolids[i] << "\')";
	}
	// Objetos
	for(j = 0; j < vecObjects.size(); j++)
	{
		if(j == 0 && i > 0) { ssInitMapContent << ", \n"; i=0; }
		if(j > 0) ssInitMapContent << ", \n";
		ssInitMapContent << "\tcreateGameEntity(OID_" << j << ", \'O\', object, "
		                 << vecObjects[j].x << ", " << vecObjects[j].y
		                 << ", inactive, norule, \n";
		ssInitMapContent << "\t\t\t[name(\'ObjectName\'), solid(false), "
				 << "static(false), use_rule(norule)," << endl
				 << "\t\t\tdescription(\'Object Description\'), appearance(attribs(normal, default, default))])";
	}
	// Enemigos
	for(k = 0; k < vecEnemies.size(); k++)
	{
		if(k == 0 && (j > 0 || (j==0 && i>0))) { ssInitMapContent << ", \n"; j=0; }
		if(k > 0) ssInitMapContent << ", \n";
		ssInitMapContent << "\tcreateGameEntity(EID_"<< k <<", \'E\', mortal, "
		                 << vecEnemies[k].x << ", " << vecEnemies[k].y
		                 << ", active, norule, [appearance(attribs(normal, default, default))])";
	}
	string sContent = ssInitMapContent.str();
	if (sContent.length() == 0)
	   cout << ".\n";
	else
	   cout << ":- \n" << sContent << ".\n";

	cout << "norule(_).\n";
	cout << "norule(_,_,_,_,_).\n";
	fentrada.close();

	return 0;
}
