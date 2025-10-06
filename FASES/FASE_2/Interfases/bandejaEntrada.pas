unit bandejaEntrada;

{$MODE DELPHI}

interface
    procedure showBandejaEntradaWindow;

implementation
    uses
        SysUtils, gtk2, glib2, gdk2, variables, InterfaceTools, ListaDoble, ListaSimple, Pila, ArbolB, verFavoritos;

    var
        bandejaEntradaWindow, detalleWindow: PGtkWidget;
        treeView: PGtkWidget;
        listStore: PGtkListStore;
        

    procedure eliminarCorreo(widget: PGtkWidget; data: gpointer); cdecl;
        var 
            correoEliminar: PCorreo;
            pilaUsuario: PPila;
        begin
            correoEliminar := PCorreo(data);
            pilaUsuario := usuarioActual^.papelera;

            //Insertar a la pila
            insertarPila(pilaUsuario, correoEliminar^.idCorreo, correoEliminar^.remitente, correoEliminar^.estado, correoEliminar^.programado, correoEliminar^.asunto, correoEliminar^.fecha, correoEliminar^.mensaje);
            usuarioActual^.papelera := pilaUsuario;

            // Eliminar de la lista de correos
            if correoEliminar^.anterior <> nil then
                correoEliminar^.anterior^.siguiente := correoEliminar^.siguiente
            else
                usuarioActual^.correos := correoEliminar^.siguiente; 

            if correoEliminar^.siguiente <> nil then
            correoEliminar^.siguiente^.anterior := correoEliminar^.anterior;

            Dispose(correoEliminar);
            imprimirPila(usuarioActual^.papelera);
        end;

    function contarCorreosNoLeidos(lista: PCorreo): Integer;
        var
            actual: PCorreo;
            contador: Integer;
        begin
            actual := lista;
            contador := 0;
            while actual <> nil do
            begin
                if actual^.estado = 'NL' then
                    Inc(contador);
                actual := actual^.siguiente;
            end;
            Result := contador;
        end;

    procedure actualizarContadorNoLeidos(labelNoLeidos: PGtkWidget);
    var
        cantidadNoLeidos: Integer;
        labelTexto: String;
    begin
        cantidadNoLeidos := contarCorreosNoLeidos(usuarioActual^.correos);
        labelTexto := Format('No Leídos: %d', [cantidadNoLeidos]);
        gtk_label_set_text(GTK_LABEL(labelNoLeidos), PChar(labelTexto));
    end;

    procedure agregarAFavoritos(widget: PGtkWidget; data: gpointer); cdecl;
    var
        correo : PCorreo;
        correoFavorito: TCorreoFavorito;
    begin
        correo := PCorreo(data);

        //Creamos el registro para el arbol B
        correoFavorito.idCorreo := correo^.idCorreo;
        correoFavorito.asunto := correo^.asunto;
        correoFavorito.remitente := correo^.remitente;
        correoFavorito.destinatario := usuarioActual^.email;
        correoFavorito.mensaje := correo^.mensaje;

        //Insertamos en el arbol B
        insertarFavorito(correoFavorito);
        imprimirArbol(usuarioActual^.favoritos);
        gtk_widget_destroy(detalleWindow);
        end;

    procedure mostrarDetalleCorreo(correo: PCorreo);
        var
            grid, btnEliminar, btnFavoritos: PGtkWidget;
        begin
            detalleWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
            gtk_window_set_title(GTK_WINDOW(detalleWindow), 'Detalle del Correo');
            gtk_container_set_border_width(GTK_CONTAINER(detalleWindow), 10);
            gtk_window_set_default_size(GTK_WINDOW(detalleWindow), 400, 300);

            grid := gtk_table_new(5, 2, False);
            gtk_container_add(GTK_CONTAINER(detalleWindow), grid);

            gtk_table_attach_defaults(GTK_TABLE(grid), gtk_label_new('Remitente:'), 0, 1, 0, 1);
            gtk_table_attach_defaults(GTK_TABLE(grid), gtk_label_new(PChar(correo^.remitente)), 1, 2, 0, 1);
            gtk_table_attach_defaults(GTK_TABLE(grid), gtk_label_new('Asunto:'), 0, 1, 1, 2);
            gtk_table_attach_defaults(GTK_TABLE(grid), gtk_label_new(PChar(correo^.asunto)), 1, 2, 1, 2);
            gtk_table_attach_defaults(GTK_TABLE(grid), gtk_label_new('Fecha:'), 0, 1, 2, 3);
            gtk_table_attach_defaults(GTK_TABLE(grid), gtk_label_new(PChar(DateTimeToStr(correo^.fecha))), 1, 2, 2, 3);
            gtk_table_attach_defaults(GTK_TABLE(grid), gtk_label_new('Mensaje:'), 0, 1, 3, 4);
            gtk_table_attach_defaults(GTK_TABLE(grid), gtk_label_new(PChar(correo^.mensaje)), 1, 2, 3, 4);

            btnEliminar := gtk_button_new_with_label('Eliminar');
            g_signal_connect(btnEliminar, 'clicked', G_CALLBACK(@eliminarCorreo), correo);
            gtk_table_attach_defaults(GTK_TABLE(grid), btnEliminar, 0, 2, 4, 5);

            btnFavoritos := gtk_button_new_with_label('Favoritos');
            g_signal_connect(btnFavoritos, 'clicked', G_CALLBACK(@agregarAFavoritos), correo);
            gtk_table_attach_defaults(GTK_TABLE(grid), btnFavoritos, 0, 2, 5, 6);

            gtk_widget_show_all(detalleWindow);
        end;

    function obtenerCorreoPorIndice(lista: PCorreo; indice: Integer): PCorreo;
        var
            actual: PCorreo;
            contador: Integer;
        begin
            actual := lista;
            contador := 0;
            while (actual <> nil) and (contador < indice) do
            begin
                actual := actual^.siguiente;
                Inc(contador);
            end;
            Result := actual;
        end;

    
    procedure onCorreoSeleccionado(treeView: PGtkWidget; path: PGtkTreePath; column: PGtkTreeViewColumn; TUserData: gpointer); cdecl;
        var
            indice: Integer;
            correo: PCorreo;
        begin
            indice := StrToInt(gtk_tree_path_to_string(path));
            correo := obtenerCorreoPorIndice(usuarioActual^.correos, indice);
            if correo <> nil then
                correo^.estado := 'L'; // Marcar como leído
                mostrarDetalleCorreo(correo);
        end;

    procedure showBandejaEntradaWindow;
    var
        grid: PGtkWidget;
        actual: PCorreo;
        iter: TGtkTreeIter;
        colRemitente, colAsunto, colEstado: PGtkTreeViewColumn;
        labelNoLeidos: PGtkWidget;
    begin
        gtk_init(@argc, @argv);
        bandejaEntradaWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
        gtk_window_set_title(GTK_WINDOW(bandejaEntradaWindow), 'Bandeja de Entrada');
        gtk_container_set_border_width(GTK_CONTAINER(bandejaEntradaWindow), 10);
        gtk_window_set_default_size(GTK_WINDOW(bandejaEntradaWindow), 600, 400);

        grid := gtk_table_new(2, 1, False);
        gtk_container_add(GTK_CONTAINER(bandejaEntradaWindow), grid);

        // Etiqueta para correos no leídos
        labelNoLeidos := gtk_label_new('No Leídos: 0');
        actualizarContadorNoLeidos(labelNoLeidos);
        gtk_table_attach(GTK_TABLE(grid), labelNoLeidos, 0, 1, 0, 1, GTK_FILL, GTK_FILL, 0, 0);

        // Crear el modelo y el treeView
        listStore := gtk_list_store_new(3, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING);
        treeView := gtk_tree_view_new_with_model(GTK_TREE_MODEL(listStore));

        // Crear y agregar columnas al treeView
        colEstado := gtk_tree_view_column_new_with_attributes('Estado', gtk_cell_renderer_text_new(), 'text', 0, nil);
        colAsunto := gtk_tree_view_column_new_with_attributes('Asunto', gtk_cell_renderer_text_new(), 'text', 1, nil);
        colRemitente := gtk_tree_view_column_new_with_attributes('Remitente', gtk_cell_renderer_text_new(), 'text', 2, nil);

        gtk_tree_view_append_column(GTK_TREE_VIEW(treeView), colEstado);
        gtk_tree_view_append_column(GTK_TREE_VIEW(treeView), colAsunto);
        gtk_tree_view_append_column(GTK_TREE_VIEW(treeView), colRemitente);

    // Agregar solo el treeView a la tabla
        gtk_table_attach_defaults(GTK_TABLE(grid), treeView, 0, 1, 1, 2);

        //Datos
        actual := usuarioActual^.correos;
        while actual <> nil do
        begin
            
            gtk_list_store_append(GTK_LIST_STORE(listStore), @iter);
            gtk_list_store_set(GTK_LIST_STORE(listStore), @iter,
                0, PChar(actual^.estado),
                1, PChar(actual^.asunto),
                2, PChar(actual^.remitente),
            -1);
            actual := actual^.siguiente;
        end;

        g_signal_connect(treeView, 'row-activated', G_CALLBACK(@onCorreoSeleccionado), nil);

        gtk_widget_show_all(bandejaEntradaWindow);

        g_signal_connect(bandejaEntradaWindow, 'destroy', G_CALLBACK(@gtk_main_quit), nil);

        gtk_main;
    end;
end.