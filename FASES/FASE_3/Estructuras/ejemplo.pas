procedure EscribirLinea(var f: Text; const linea: String);
begin
  Writeln(f, linea); // Escribir línea en el archivo
end;

// Procedimiento para generar nodos y conexiones en recorrido inorden
procedure GenerarNodosInOrdenDOT(var f: Text; nodo: PNodo);
begin
  if nodo = nil then
    Exit; // Salir si el nodo es nulo

  // Recorrer subárbol izquierdo
  GenerarNodosInOrdenDOT(f, nodo^.izquierda);

  // Escribir nodo actual en formato DOT con altura
  EscribirLinea(f, Format('  "%d" [label="%d: %s (Alt: %d)"];', [nodo^.clave, nodo^.clave, nodo^.valor, nodo^.altura]));

  // Conexión al hijo izquierdo
  if nodo^.izquierda <> nil then
    EscribirLinea(f, Format('  "%d" -> "%d";', [nodo^.clave, nodo^.izquierda^.clave]));

  // Conexión al hijo derecho
  if nodo^.derecha <> nil then
    EscribirLinea(f, Format('  "%d" -> "%d";', [nodo^.clave, nodo^.derecha^.clave]));

  // Recorrer subárbol derecho
  GenerarNodosInOrdenDOT(f, nodo^.derecha);
end;

// Generar archivo .dot para recorrido inorden
procedure GenerarDOTInOrden(raiz: PNodo; const nombreArchivo: String);
var
  f: Text; // Archivo de texto
begin
  Assign(f, nombreArchivo); // Asignar archivo
  Rewrite(f);               // Crear o sobrescribir archivo

  EscribirLinea(f, 'digraph ArbolAVLInOrden {'); // Iniciar grafo
  EscribirLinea(f, '  node [shape=circle, style=filled, color=lightblue];'); // Estilo de nodos

  if raiz <> nil then
    GenerarNodosInOrdenDOT(f, raiz) // Generar nodos si el árbol no está vacío
  else
    EscribirLinea(f, '  vacio [label="(vacio)"];'); // Árbol vacío

  EscribirLinea(f, '}'); // Cerrar grafo
  Close(f);              // Cerrar archivo
  Writeln('Archivo DOT generado (InOrden): ', nombreArchivo);
end;

// Programa principal
begin
  // Generar archivo DOT para recorrido inorden
  GenerarDOTInOrden(raiz, 'arbol_avl_inorden.dot');
  Writeln('Archivo generado para el recorrido inorden.');
end.