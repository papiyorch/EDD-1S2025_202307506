unit papelera;

{$MODE DELPHI}
interface
    procedure showPapeleraWindow;

implementation
    uses
        SysUtils, gtk2, glib2, gdk2, variables, Pila, InterfaceTools, ListaSimple;
        var
            papeleraWindow: PGtkWidget;
            btnDelete: PGtkWidget;
            treeView: PGtkWidget;
            listStore: PGtkListStore;

        procedure eliminarCorreoPapelera(widget: PGtkWidget; data: gpointer); cdecl;
        begin
            eliminarPila(usuarioActual^.papelera);
        end;

        function obtenerCorreo(pila: PPila; indice: Integer): PPila;
        var
            actual: PPila;
            contador: Integer;
        begin
            actual := pila;
            contador := 0;
            while (actual <> nil) and (contador < indice) do
            begin
                actual := actual^.siguiente;
                Inc(contador);
            end;
            Result := actual;
        end;

        procedure onCorreoPapelera(treeView: PGtkWidget; path: PGtkTreePath; column: PGtkTreeViewColumn; user_data: gpointer); cdecl;
        var
            indice: Integer;
            actual: PPila;
        begin
            indice := StrToInt(gtk_tree_path_to_string(path));
            actual := obtenerCorreo(usuarioActual^.papelera, indice);
        end;

        procedure showPapeleraWindow;
        var
            grid: PGtkWidget;
            actual: PPila;
            iter: TGtkTreeIter;
            colAsunto, colRemitente, colMensaje: PGtkTreeViewColumn;
        
        begin
            gtk_init(@argc, @argv);

            papeleraWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
            gtk_window_set_title(GTK_WINDOW(papeleraWindow), 'Papelera');
            gtk_container_set_border_width(GTK_CONTAINER(papeleraWindow), 10);
            gtk_window_set_default_size(GTK_WINDOW(papeleraWindow), 600, 400);

            grid := gtk_table_new(2, 1, False);
            gtk_container_add(GTK_CONTAINER(papeleraWindow), grid);

            // Crear el TreeView y el ListStore
            listStore := gtk_list_store_new(3, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING);
            treeView := gtk_tree_view_new_with_model(GTK_TREE_MODEL(listStore));
            
            // Crear columnas
            colAsunto := gtk_tree_view_column_new_with_attributes('Asunto', gtk_cell_renderer_text_new(), 'text', 0, nil);
            colRemitente := gtk_tree_view_column_new_with_attributes('Remitente', gtk_cell_renderer_text_new(), 'text', 1, nil);
            colMensaje := gtk_tree_view_column_new_with_attributes('Mensaje', gtk_cell_renderer_text_new(), 'text', 2, nil);

            // Añadir columnas al TreeView
            gtk_tree_view_append_column(GTK_TREE_VIEW(treeView), colAsunto);
            gtk_tree_view_append_column(GTK_TREE_VIEW(treeView), colRemitente);
            gtk_tree_view_append_column(GTK_TREE_VIEW(treeView), colMensaje);

            //Agregar el TreeView a la tabla
            gtk_table_attach_defaults(GTK_TABLE(grid), treeView, 0, 1, 0, 1);

            // Llenar el ListStore con los datos de la pila
            actual := usuarioActual^.papelera;
            while actual <> nil do
            begin
                gtk_list_store_append(GTK_LIST_STORE(listStore), @iter);
                gtk_list_store_set(GTK_LIST_STORE(listStore), @iter,
                    0, PChar(actual^.asunto),
                    1, PChar(actual^.remitente),
                    2, PChar(actual^.mensaje),
                    -1);
                actual := actual^.siguiente;
            end;

            g_signal_connect(treeView, 'row-activated', G_CALLBACK(@onCorreoPapelera), nil);
            
            g_signal_connect(papeleraWindow, 'destroy', G_CALLBACK(@gtk_main_quit), nil);
    
            btnDelete := gtk_button_new_with_label('Eliminar');
            gtk_table_attach(GTK_TABLE(grid),btnDelete,0,1,1,2,GTK_SHRINK,GTK_SHRINK,5,5);

            g_signal_connect(btnDelete, 'clicked', G_CALLBACK(@eliminarCorreoPapelera), usuarioActual^.papelera);

            gtk_widget_show_all(papeleraWindow);
            gtk_main;

        end;

end.