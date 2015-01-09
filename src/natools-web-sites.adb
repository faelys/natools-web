------------------------------------------------------------------------------
-- Copyright (c) 2014, Natacha Porté                                        --
--                                                                          --
-- Permission to use, copy, modify, and distribute this software for any    --
-- purpose with or without fee is hereby granted, provided that the above   --
-- copyright notice and this permission notice appear in all copies.        --
--                                                                          --
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES --
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF         --
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR  --
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES   --
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN    --
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF  --
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.           --
------------------------------------------------------------------------------

with Ada.Directories;
with Natools.S_Expressions.Atom_Ref_Constructors;
with Natools.S_Expressions.File_Readers;
with Natools.S_Expressions.Interpreter_Loop;
with Natools.Static_Maps.Web.Sites;
with Natools.Web.Error_Pages;

package body Natools.Web.Sites is

   procedure Add_Page
     (Builder : in out Site_Builder;
      Constructor : in Page_Constructor;
      File_Root : in S_Expressions.Atom;
      Path_Root : in S_Expressions.Atom);

   procedure Add_Page
     (Builder : in out Site_Builder;
      Constructor : in Page_Constructor;
      File_Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class);

   procedure Add_Page_Simple
     (Builder : in out Site_Builder;
      Constructor : in Page_Constructor;
      Common_Name : in S_Expressions.Atom);

   procedure Get_Page
     (Container : in Page_Maps.Updatable_Map;
      Path : in S_Expressions.Atom;
      Result : out Page_Maps.Cursor;
      Extra_Path_First : out S_Expressions.Offset);

   procedure Execute
     (Builder : in out Site_Builder;
      Context : in Meaningless_Type;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class);

   procedure Set_If_Possible
     (Reference : in out S_Expressions.Atom_Refs.Immutable_Reference;
      Expression : in S_Expressions.Lockable.Descriptor'Class);


   ------------------------------
   -- Local Helper Subprograms --
   ------------------------------

   procedure Get_Page
     (Container : in Page_Maps.Updatable_Map;
      Path : in S_Expressions.Atom;
      Result : out Page_Maps.Cursor;
      Extra_Path_First : out S_Expressions.Offset)
   is
      use type S_Expressions.Atom;
      use type S_Expressions.Octet;
      use type S_Expressions.Offset;
   begin
      Result := Container.Floor (Path);

      if not Page_Maps.Has_Element (Result) then
         Extra_Path_First := 0;
         return;
      end if;

      declare
         Found_Path : constant S_Expressions.Atom := Page_Maps.Key (Result);
      begin
         if Found_Path'Length > Path'Length
           or else Path (Path'First .. Path'First + Found_Path'Length - 1)
              /= Found_Path
         then
            Page_Maps.Clear (Result);
            return;
         end if;

         Extra_Path_First := Path'First + Found_Path'Length;
      end;

      if Extra_Path_First in Path'Range
        and then Path (Extra_Path_First) /= Character'Pos ('/')
      then
         Page_Maps.Clear (Result);
         return;
      end if;
   end Get_Page;


   procedure Set_If_Possible
     (Reference : in out S_Expressions.Atom_Refs.Immutable_Reference;
      Expression : in S_Expressions.Lockable.Descriptor'Class)
   is
      use type S_Expressions.Events.Event;
   begin
      if Expression.Current_Event = S_Expressions.Events.Add_Atom then
         Reference := S_Expressions.Atom_Ref_Constructors.Create
           (Expression.Current_Atom);
      end if;
   end Set_If_Possible;



   ----------------------
   -- Site Interpreter --
   ----------------------

   procedure Add_Page
     (Builder : in out Site_Builder;
      Constructor : in Page_Constructor;
      File_Root : in S_Expressions.Atom;
      Path_Root : in S_Expressions.Atom)
   is
      use type S_Expressions.Atom;

      function Get_Loader (File : S_Expressions.Atom) return Page_Loader'Class;

      function Get_Loader
        (File : S_Expressions.Atom) return Page_Loader'Class
      is
         Cursor : constant Page_Loaders.Cursor
           := Builder.Old_Loaders.Find (File);
      begin
         if Page_Loaders.Has_Element (Cursor) then
            return Page_Loaders.Element (Cursor);
         else
            return Constructor (File);
         end if;
      end Get_Loader;

      File : constant S_Expressions.Atom
        := Builder.File_Prefix.Query.Data.all
         & File_Root
         & Builder.File_Suffix.Query.Data.all;
      Path : constant S_Expressions.Atom
        := Builder.Path_Prefix.Query.Data.all
         & Path_Root
         & Builder.Path_Suffix.Query.Data.all;

      Loader : Page_Loader'Class := Get_Loader (File);
   begin
      Load (Loader, Builder, Path);
      Builder.New_Loaders.Insert (File, Loader);
   end Add_Page;


   procedure Add_Page
     (Builder : in out Site_Builder;
      Constructor : in Page_Constructor;
      File_Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class)
   is
      use type S_Expressions.Events.Event;
   begin
      if Arguments.Current_Event = S_Expressions.Events.Add_Atom then
         Add_Page (Builder, Constructor, File_Name, Arguments.Current_Atom);
      end if;
   end Add_Page;


   procedure Add_Page_Simple
     (Builder : in out Site_Builder;
      Constructor : in Page_Constructor;
      Common_Name : in S_Expressions.Atom) is
   begin
      Add_Page (Builder, Constructor, Common_Name, Common_Name);
   end Add_Page_Simple;


   procedure Add_Pages is new S_Expressions.Interpreter_Loop
     (Site_Builder, Page_Constructor, Add_Page, Add_Page_Simple);


   procedure Execute
     (Builder : in out Site_Builder;
      Context : in Meaningless_Type;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class)
   is
      pragma Unreferenced (Context);
      package Commands renames Natools.Static_Maps.Web.Sites;
      use type S_Expressions.Events.Event;
      use type S_Expressions.Atom;
      use type S_Expressions.Octet;
      use type S_Expressions.Offset;
   begin
      case Commands.To_Command (S_Expressions.To_String (Name)) is
         when Commands.Error =>
            declare
               Cursor : Page_Constructors.Cursor
                 := Builder.Constructors.Find (Name);
            begin
               if not Page_Constructors.Has_Element (Cursor)
                 and then Name'Length > 1
                 and then Name (Name'Last) = Character'Pos ('s')
               then
                  Cursor := Builder.Constructors.Find
                    (Name (Name'First .. Name'Last - 1));
               end if;

               if Page_Constructors.Has_Element (Cursor) then
                  Add_Pages
                    (Arguments,
                     Builder,
                     Page_Constructors.Element (Cursor));
               else
                  Log (Severities.Error, "Unknown site command """
                    & S_Expressions.To_String (Name) & '"');
               end if;
            end;

         when Commands.Set_Default_Template =>
            Set_If_Possible (Builder.Default_Template, Arguments);

         when Commands.Set_File_Prefix =>
            Set_If_Possible (Builder.File_Prefix, Arguments);

         when Commands.Set_File_Suffix =>
            Set_If_Possible (Builder.File_Suffix, Arguments);

         when Commands.Set_Named_Element_File =>
            if Arguments.Current_Event = S_Expressions.Events.Add_Atom then
               declare
                  Reader : S_Expressions.File_Readers.S_Reader
                    := S_Expressions.File_Readers.Reader
                       (S_Expressions.To_String (Arguments.Current_Atom));
               begin
                  Containers.Set_Expression_Maps
                    (Builder.Named_Elements, Reader);
               end;
            end if;

         when Commands.Set_Named_Elements =>
            Containers.Set_Expression_Maps (Builder.Named_Elements, Arguments);

         when Commands.Set_Path_Prefix =>
            Set_If_Possible (Builder.Path_Prefix, Arguments);

         when Commands.Set_Path_Suffix =>
            Set_If_Possible (Builder.Path_Suffix, Arguments);

         when Commands.Set_Static_Paths =>
            Containers.Append_Atoms (Builder.Static, Arguments);

         when Commands.Set_Template_File =>
            if Arguments.Current_Event = S_Expressions.Events.Add_Atom then
               declare
                  Reader : S_Expressions.File_Readers.S_Reader
                    := S_Expressions.File_Readers.Reader
                       (S_Expressions.To_String (Arguments.Current_Atom));
               begin
                  Containers.Set_Expressions (Builder.Templates, Reader);
               end;
            end if;

         when Commands.Set_Templates =>
            Containers.Set_Expressions (Builder.Templates, Arguments);
      end case;
   end Execute;


   procedure Interpreter is new S_Expressions.Interpreter_Loop
     (Site_Builder, Meaningless_Type, Execute);


   procedure Insert
     (Builder : in out Site_Builder;
      Path : in S_Expressions.Atom;
      New_Page : in Page'Class) is
   begin
      Builder.Pages.Insert (Path, New_Page);
   end Insert;


   procedure Insert
     (Builder : in out Site_Builder;
      Tags : in Web.Tags.Tag_List;
      Visible : in Web.Tags.Visible'Class) is
   begin
      Web.Tags.Register (Builder.Tags, Tags, Visible);
   end Insert;


   procedure Update
     (Builder : in out Site_Builder;
      Expression : in out S_Expressions.Lockable.Descriptor'Class) is
   begin
      Interpreter (Expression, Builder, Meaningless_Value);
   end Update;



   ---------------------------
   -- Site Public Interface --
   ---------------------------

   procedure Register
     (Self : in out Site;
      Name : in String;
      Constructor : in Page_Constructor) is
   begin
      Self.Constructors.Insert (S_Expressions.To_Atom (Name), Constructor);
   end Register;


   procedure Reload (Object : in out Site) is
      Reader : S_Expressions.File_Readers.S_Reader
        := S_Expressions.File_Readers.Reader
           (S_Expressions.To_String (Object.File_Name.Query.Data.all));
      Empty_Atom : constant S_Expressions.Atom_Refs.Immutable_Reference
        := S_Expressions.Atom_Ref_Constructors.Create
           (S_Expressions.Null_Atom);
      Builder : Site_Builder
        := (Constructors => Object.Constructors'Access,
            Default_Template
              => S_Expressions.Atom_Refs.Null_Immutable_Reference,
            File_Prefix => Empty_Atom,
            File_Suffix => Empty_Atom,
            New_Loaders => <>,
            Old_Loaders => Object.Loaders,
            Path_Prefix => Empty_Atom,
            Path_Suffix => Empty_Atom,
            Named_Elements | Pages | Static | Tags | Templates => <>);
   begin
      Update (Builder, Reader);

      Object.Default_Template := Builder.Default_Template;
      Object.Loaders := Page_Loaders.Create (Builder.New_Loaders);
      Object.Named_Elements := Builder.Named_Elements;
      Object.Pages := Page_Maps.Create (Builder.Pages);
      Object.Static := Containers.Create (Builder.Static);
      Object.Tags := Tags.Create (Builder.Tags);
      Object.Templates := Builder.Templates;

      if Object.Default_Template.Is_Empty then
         Object.Default_Template := S_Expressions.Atom_Ref_Constructors.Create
           (S_Expressions.To_Atom ("html"));
      end if;
   end Reload;


   procedure Reset (Object : in out Site; File_Name : in String) is
   begin
      Object.File_Name := S_Expressions.Atom_Ref_Constructors.Create
        (S_Expressions.To_Atom (File_Name));
      Reload (Object);
   end Reset;


   procedure Respond
     (Object : in out Site;
      Exchange : aliased in out Exchanges.Exchange)
   is
      use type S_Expressions.Octet;
      use type S_Expressions.Offset;

      Extended_Exchange : Sites.Exchange (Exchange'Access, Object'Access);

      procedure Call_Page
        (Key : in S_Expressions.Atom;
         Page_Object : in out Page'Class);

      procedure Send_File_If_Exists (In_Directory : in S_Expressions.Atom);

      Path : constant S_Expressions.Atom
        := S_Expressions.To_Atom (Exchanges.Path (Exchange));

      procedure Call_Page
        (Key : in S_Expressions.Atom;
         Page_Object : in out Page'Class)
      is
         use type S_Expressions.Atom;
      begin
         pragma Assert (Key'Length <= Path'Length
           and then Key = Path (Path'First .. Path'First + Key'Length - 1));

         Respond
           (Page_Object,
            Extended_Exchange,
            Path (Path'First + Key'Length .. Path'Last));
      end Call_Page;

      procedure Send_File_If_Exists (In_Directory : in S_Expressions.Atom) is
         use type S_Expressions.Atom;
         Candidate_Name : constant S_Expressions.Atom := In_Directory & Path;
      begin
         if Ada.Directories.Exists
              (S_Expressions.To_String (Candidate_Name))
         then
            Exchanges.Send_File (Exchange, Candidate_Name);
         end if;
      end Send_File_If_Exists;

      Cursor : Page_Maps.Cursor;
      Extra_Path_First : S_Expressions.Offset;
   begin
      if not Object.Static.Is_Empty then
         for Path_Ref of Object.Static.Query.Data.all loop
            Send_File_If_Exists (Path_Ref.Query.Data.all);

            if Exchanges.Has_Response (Exchange) then
               return;
            end if;
         end loop;
      end if;

      Get_Page (Object.Pages, Path, Cursor, Extra_Path_First);

      if not Page_Maps.Has_Element (Cursor) then
         Error_Pages.Not_Found (Extended_Exchange);
         return;
      end if;

      Response_Loop :
      loop
         Object.Pages.Update_Element (Cursor, Call_Page'Access);

         exit Response_Loop when Exchanges.Has_Response (Exchange);

         Find_Parent :
         loop
            Remove_Path_Component :
            loop
               Extra_Path_First := Extra_Path_First - 1;

               exit Response_Loop
                 when Extra_Path_First not in Path'Range;
               exit Remove_Path_Component
                 when Path (Extra_Path_First) = Character'Pos ('/');
            end loop Remove_Path_Component;

            Cursor := Object.Pages.Find
              (Path (Path'First .. Extra_Path_First - 1));
            exit Find_Parent when Page_Maps.Has_Element (Cursor);
         end loop Find_Parent;
      end loop Response_Loop;

      if not Exchanges.Has_Response (Exchange) then
         Error_Pages.Not_Found (Extended_Exchange);
      end if;
   end Respond;



   -------------------------
   -- Site Data Accessors --
   -------------------------

   function Get_Tags (Object : Site) return Tags.Tag_DB is
   begin
      return Object.Tags;
   end Get_Tags;


   procedure Get_Template
     (Object : in Site;
      Name : in S_Expressions.Atom;
      Template : out S_Expressions.Caches.Cursor;
      Found : out Boolean)
   is
      Cursor : constant Containers.Expression_Maps.Cursor
        := Object.Templates.Find (Name);
   begin
      Found := Containers.Expression_Maps.Has_Element (Cursor);

      if Found then
         Template := Containers.Expression_Maps.Element (Cursor);
      end if;
   end Get_Template;


   function Get_Template
     (Object : in Site;
      Elements : in Containers.Expression_Maps.Constant_Map;
      Expression : in out S_Expressions.Lockable.Descriptor'Class;
      Name : in S_Expressions.Atom := S_Expressions.Null_Atom;
      Lookup_Template : in Boolean := True;
      Lookup_Element : in Boolean := True;
      Lookup_Name : in Boolean := False)
     return S_Expressions.Caches.Cursor
   is
      Cursor : Containers.Expression_Maps.Cursor;

      use type S_Expressions.Events.Event;
   begin
      --  Check whether Name should be extracted from Expression

      if Name'Length = 0
        and then (not Lookup_Name)
        and then (Lookup_Template or Lookup_Element)
        and then Expression.Current_Event = S_Expressions.Events.Add_Atom
      then
         declare
            Actual_Name : constant S_Expressions.Atom
              := Expression.Current_Atom;
         begin
            Expression.Next;
            return Get_Template
              (Object, Elements, Expression,
               Actual_Name,
               Lookup_Template, Lookup_Element, True);
         end;
      end if;

      --  Try Name among elements

      Cursor := Elements.Find (Name);

      if Containers.Expression_Maps.Has_Element (Cursor) then
         return Containers.Expression_Maps.Element (Cursor);
      end if;

      --  Try Name among templates

      Cursor := Object.Templates.Find (Name);

      if Containers.Expression_Maps.Has_Element (Cursor) then
         return Containers.Expression_Maps.Element (Cursor);
      end if;

      --  Fallback on Expression, converting it destructively if needed

      if Expression in S_Expressions.Caches.Cursor then
         return S_Expressions.Caches.Cursor (Expression);
      else
         return S_Expressions.Caches.Move (Expression);
      end if;
   end Get_Template;


   function Get_Template
     (Object : in Site;
      Element_Map_Name : in S_Expressions.Atom;
      Expression : in out S_Expressions.Lockable.Descriptor'Class;
      Name : in S_Expressions.Atom := S_Expressions.Null_Atom;
      Lookup_Template : in Boolean := True;
      Lookup_Element : in Boolean := True;
      Lookup_Name : in Boolean := False)
     return S_Expressions.Caches.Cursor
   is
      Elements : Containers.Expression_Maps.Constant_Map;
      Cursor : constant Containers.Expression_Map_Maps.Cursor
        := Object.Named_Elements.Find (Element_Map_Name);
   begin
      if Containers.Expression_Map_Maps.Has_Element (Cursor) then
         Elements := Containers.Expression_Map_Maps.Element (Cursor);
      end if;

      return Get_Template
        (Object,
         Elements,
         Expression,
         Name,
         Lookup_Template,
         Lookup_Element,
         Lookup_Name);
   end Get_Template;


   function Default_Template (Object : Site) return S_Expressions.Atom is
   begin
      return Object.Default_Template.Query.Data.all;
   end Default_Template;

end Natools.Web.Sites;
