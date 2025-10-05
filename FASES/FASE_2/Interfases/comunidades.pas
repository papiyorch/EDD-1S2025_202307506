unit comunidades;

{$MODE DELPHI} 
interface
    
uses
        SysUtils, Classes, InterfaceTools, ListaSimple, gtk2, glib2, gdk2, variables, listaDeLista;
procedure showComunidadesWindow;

var 
    listaComunidades: TComunidadList;

implementation
    
    var
        comunidadesWindow: PGtkWidget;
        entryNombreComunidad, entryEmailUsuario: PGtkWidget;
        btnCrearComunidad, btnAgregarAComunidad: PGtkWidget;
        comboComunidades: PGtkWidget;

    procedure limpiarCombo;
    var
        count, i: Integer;
    begin
        count := gtk_tree_model_iter_n_children(gtk_combo_box_get_model(GTK_COMBO_BOX(comboComunidades)), nil);
        for i := count-1 downto 0 do
            gtk_combo_box_remove_text(GTK_COMBO_BOX(comboComunidades), i);
    end;

    procedure actualizarComboComunidades;
    var
        comunidadActual: PComunidad;
    begin
        limpiarCombo;
        comunidadActual := listaComunidades.cabeza;
        while comunidadActual <> nil do
        begin
            gtk_combo_box_append_text(GTK_COMBO_BOX(comboComunidades), PChar(comunidadActual^.nombre));
            comunidadActual := comunidadActual^.siguiente;
        end;
    end;

    procedure onCrearComunidadClicked(widget: PGtkWidget; data: gpointer); cdecl;
    var
        nombreComunidad: PChar;
    begin
        nombreComunidad := gtk_entry_get_text(GTK_ENTRY(entryNombreComunidad));

        if (nombreComunidad <> nil) and (String(nombreComunidad) <> '') then
        begin
            // Si no existe la comunidad, la agregamos
            if comunidadPorNombre(listaComunidades, String(nombreComunidad)) = nil then
            begin
                agregarComunidad(listaComunidades, String(nombreComunidad), ''); 
                actualizarComboComunidades;
                gtk_combo_box_set_active(GTK_COMBO_BOX(comboComunidades), listaComunidades.contador - 1);
                gtk_entry_set_text(GTK_ENTRY(entryNombreComunidad), '');
            end
            else
                mostrarMensajeError(comunidadesWindow, 'Error', 'La comunidad ya existe.');
        end;
    end;

    procedure onAgregarAComunidadClicked(widget: PGtkWidget; data: gpointer); cdecl;
    var
        emailUsuario: PChar;
        nombreComunidad: PChar;
        nodoUsuario: PNodo;
        comunidad: PComunidad;
    begin
        emailUsuario := gtk_entry_get_text(GTK_ENTRY(entryEmailUsuario));
        nombreComunidad := gtk_combo_box_get_active_text(GTK_COMBO_BOX(comboComunidades));
        if (emailUsuario = nil) or (nombreComunidad = nil) then
            Exit;

    //verificamos si el usuario existe 
        nodoUsuario := listaUsuarios;
        while nodoUsuario <> nil do
        begin
            if nodoUsuario^.email = String(emailUsuario) then
                begin
                    comunidad := comunidadPorNombre(listaComunidades, String(nombreComunidad));
                    if comunidad = nil then
                    begin
                        mostrarMensajeError(comunidadesWindow, 'Error', 'La comunidad no existe.');
                        Exit;
                    end;
                    //Verificamos si el usuario ya está en la comunidad
                    if existeUsuarioEnComunidad(comunidad^.usuarios, String(emailUsuario)) then
                    begin
                        mostrarMensajeError(comunidadesWindow, 'Error', 'El usuario ya pertenece a la comunidad.');
                        Exit;
                    end;
                    //Agregamos si no lo está 
                    agregarUsuarioPorNombre(listaComunidades, String(nombreComunidad), String(emailUsuario));
                    mostrarMensajeLogin(comunidadesWindow, 'Éxito', 'Usuario agregado a la comunidad.');
                    gtk_entry_set_text(GTK_ENTRY(entryEmailUsuario), '');
                    Exit;
                end;
            nodoUsuario := nodoUsuario^.siguiente;
        end;
        mostrarMensajeError(comunidadesWindow, 'Error', 'El usuario no existe.');
    end;

    procedure showComunidadesWindow;
    var
        grid: PGtkWidget;
    begin
        gtk_init(@argc, @argv);

        //Inicializamos la lista de comunidades
        inicializarComunidadList(listaComunidades);

        comunidadesWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
        gtk_window_set_title(GTK_WINDOW(comunidadesWindow), 'Comunidades');
        gtk_container_set_border_width(GTK_CONTAINER(comunidadesWindow), 10);
        gtk_window_set_default_size(GTK_WINDOW(comunidadesWindow), 400, 300);

        grid := gtk_table_new(5, 2, False);
        gtk_container_add(GTK_CONTAINER(comunidadesWindow), grid);

        entryNombreComunidad := gtk_entry_new();
        entryEmailUsuario := gtk_entry_new();
        comboComunidades := gtk_combo_box_new_text();

        btnCrearComunidad := gtk_button_new_with_label('Crear');
        btnAgregarAComunidad := gtk_button_new_with_label('Agregar');

        gtk_table_attach_defaults(GTK_TABLE(grid), gtk_label_new('Nombre:'), 0, 1, 0, 1);
        gtk_table_attach_defaults(GTK_TABLE(grid), entryNombreComunidad, 1, 2, 0, 1);
        gtk_table_attach(GTK_TABLE(grid), btnCrearComunidad, 1, 2, 1, 2, GTK_SHRINK, GTK_SHRINK, 5, 5);

        gtk_table_attach_defaults(GTK_TABLE(grid), gtk_label_new('Email:'), 0, 1, 2, 3);
        gtk_table_attach_defaults(GTK_TABLE(grid), entryEmailUsuario, 1, 2, 2, 3);

        gtk_table_attach_defaults(GTK_TABLE(grid), gtk_label_new('Comunidad:'), 0, 1, 3, 4);
        gtk_table_attach_defaults(GTK_TABLE(grid), comboComunidades, 1, 2, 3, 4);
        gtk_table_attach(GTK_TABLE(grid), btnAgregarAComunidad, 1, 2, 4, 5, GTK_SHRINK, GTK_SHRINK, 5, 5);


        //Conexiones de botones
        g_signal_connect(btnCrearComunidad, 'clicked', G_CALLBACK(@onCrearComunidadClicked), nil);
        g_signal_connect(btnAgregarAComunidad, 'clicked', G_CALLBACK(@onAgregarAComunidadClicked), nil);
        
        actualizarComboComunidades;
        gtk_widget_show_all(comunidadesWindow);
        g_signal_connect(comunidadesWindow, 'destroy', G_CALLBACK(@gtk_main_quit), nil);
        gtk_main;
    end;
end.