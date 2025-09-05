unit bandejaEntrada;

{$MODE DELPHI}

interface
    procedure showBandejaEntradaWindow;

implementation
    uses
        SysUtils, gtk2, glib2, gdk2, variables, InterfaceTools, ListaDoble, ListaSimple;

    var
        bandejaEntradaWindow, detalleWindow: PGtkWidget;
        treeView: PGtkWidget;
        listStore: PGtkListStore;
        

    procedure eliminarCorreo(widget: PGtkWidget; data: gpointer); cdecl;
        begin
            //A
        end;

    procedure mostrarDetalleCorreo(correo: PCorreo);
        var
            grid, btnEliminar: PGtkWidget;
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
    begin
        gtk_init(@argc, @argv);
        bandejaEntradaWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
        gtk_window_set_title(GTK_WINDOW(bandejaEntradaWindow), 'Bandeja de Entrada');
        gtk_container_set_border_width(GTK_CONTAINER(bandejaEntradaWindow), 10);
        gtk_window_set_default_size(GTK_WINDOW(bandejaEntradaWindow), 600, 400);

        grid := gtk_table_new(1, 1, False);
        gtk_container_add(GTK_CONTAINER(bandejaEntradaWindow), grid);

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
        gtk_table_attach_defaults(GTK_TABLE(grid), treeView, 0, 1, 0, 1);

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