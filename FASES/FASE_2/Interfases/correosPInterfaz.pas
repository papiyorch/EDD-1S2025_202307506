unit correosPInterfaz;

{$MODE DELPHI}

interface
    procedure showProgramadosWindow;

implementation
    uses 
        SysUtils, Classes, gtk2, glib2, gdk2, variables, Cola, ListaSimple, InterfaceTools, ListaDoble, ListaCircular;
    
    var
        programarWindow: PGtkWidget;
        treeView: PGtkWidget;
        listStore: PGtkListStore;
        btnEnviarProgramados: PGtkWidget;

    procedure enviarCorreosProgramados(widget: PGtkWidget; data: gpointer); cdecl;
    var
        cola: PPProgramados;
        actual: PProgramados;
        usuarioDestinoP :PNodo;
    begin

        //Convertimos el puntero a uno de cola     
        cola := PPProgramados(data);
        actual := cola^;

        while actual <> nil do
        begin
            //Busca el usaurio destino por email
            usuarioDestinoP := buscarUsuarioPorEmail(listaUsuarios, actual^.destinatario);

            if usuarioDestinoP <> nil then  
            begin
                //Inserta el correo en la lista de correos del usuario destino
                insertarDoble(usuarioDestinoP^.correos, actual^.idCorreo, actual^.remitente, 'NL', False, actual^.asunto, actual^.fechaEnvio, actual^.mensaje);
            end;
            eliminarCola(cola^);
            actual := cola^;
            
        end;
    end;

    function obtenerCorreoProgramado(cola: PProgramados; indice: Integer): PProgramados;
    var
        actual: PProgramados;
        contador: Integer;
    begin
        actual := cola;
        contador := 0;
        while (actual <> nil) and (contador < indice) do
        begin
            actual := actual^.siguiente;
            Inc(contador);
        end;
        Result := actual;
    end;

    procedure onCorreoProgramado(treeView: PGtkWidget; path: PGtkTreePath; column: PGtkTreeViewColumn; user_data: gpointer); cdecl;
    var
        indice: Integer;
        actual: PProgramados;
    begin
        indice := StrToInt(gtk_tree_path_to_string(path));
        actual := obtenerCorreoProgramado(usuarioActual^.correosProgramados, indice);
    end;

    procedure showProgramadosWindow;
    var
        grid: PGtkWidget;
        actual: PProgramados;
        iter: TGtkTreeIter;
        colAsunto, colRemitente, colFecha: PGtkTreeViewColumn;

    begin
        gtk_init(@argc, @argv);

        programarWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
        gtk_window_set_title(GTK_WINDOW(programarWindow), 'Correos Programados');
        gtk_container_set_border_width(GTK_CONTAINER(programarWindow), 10);
        gtk_window_set_default_size(GTK_WINDOW(programarWindow), 600, 400);

        grid := gtk_table_new(2, 1, False);
        gtk_container_add(GTK_CONTAINER(programarWindow), grid);

        listStore := gtk_list_store_new(3, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING);
        treeView := gtk_tree_view_new_with_model(GTK_TREE_MODEL(listStore));

        colAsunto := gtk_tree_view_column_new_with_attributes('Asunto', gtk_cell_renderer_text_new(), 'text', 0, nil);
        colRemitente := gtk_tree_view_column_new_with_attributes('Remitente', gtk_cell_renderer_text_new(), 'text', 1, nil);
        colFecha := gtk_tree_view_column_new_with_attributes('Fecha de Envío', gtk_cell_renderer_text_new(), 'text', 2, nil);

        gtk_tree_view_append_column(GTK_TREE_VIEW(treeView), colAsunto);
        gtk_tree_view_append_column(GTK_TREE_VIEW(treeView), colRemitente);
        gtk_tree_view_append_column(GTK_TREE_VIEW(treeView), colFecha);

        gtk_table_attach_defaults(GTK_TABLE(grid), treeView, 0, 1, 0, 1);

        //Lenamos la lista con los correos programados
        actual := usuarioActual^.correosProgramados;
        while actual <> nil do
        begin
            gtk_list_store_append(GTK_LIST_STORE(listStore), @iter);
            gtk_list_store_set(GTK_LIST_STORE(listStore), @iter,
                0, PChar(actual^.asunto),
                1, PChar(actual^.remitente),
                2, PChar(DateTimeToStr(actual^.fechaEnvio)),
                -1);
            actual := actual^.siguiente;
        end;

        g_signal_connect(treeView, 'row-activated', G_CALLBACK(@onCorreoProgramado), nil);

        g_signal_connect(programarWindow, 'destroy', G_CALLBACK(@gtk_main_quit), nil);

        btnEnviarProgramados := gtk_button_new_with_label('Enviar');
        gtk_table_attach(GTK_TABLE(grid), btnEnviarProgramados, 0, 1, 1, 2, GTK_SHRINK, GTK_SHRINK, 5, 5);

        g_signal_connect(btnEnviarProgramados, 'clicked', G_CALLBACK(@enviarCorreosProgramados), @usuarioActual^.correosProgramados);

        gtk_widget_show_all(programarWindow);
        gtk_main;
    end;

end.


