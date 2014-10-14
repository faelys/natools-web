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

with Natools.S_Expressions.Atom_Ref_Constructors;
with Natools.S_Expressions.File_Readers;
with Natools.S_Expressions.Interpreter_Loop;
with Natools.Static_Maps.Web.Sites;
with Natools.Web.Pages;

package body Natools.Web.Sites is

   procedure Add_Page
     (Builder : in out Site_Builder;
      File_Root : in S_Expressions.Atom;
      Path_Root : in S_Expressions.Atom);

   procedure Add_Page
     (Builder : in out Site_Builder;
      Context : in Meaningless_Type;
      File_Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class);

   procedure Add_Page_Simple
     (Builder : in out Site_Builder;
      Context : in Meaningless_Type;
      Common_Name : in S_Expressions.Atom);

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
      File_Root : in S_Expressions.Atom;
      Path_Root : in S_Expressions.Atom)
   is
      use type S_Expressions.Atom;

      File : constant S_Expressions.Atom
        := Builder.File_Prefix.Query.Data.all
         & File_Root
         & Builder.File_Suffix.Query.Data.all;
      Path : constant S_Expressions.Atom
        := Builder.Path_Prefix.Query.Data.all
         & Path_Root
         & Builder.Path_Suffix.Query.Data.all;
      Page : constant Pages.Page_Ref := Pages.Create (File, Path);
   begin
      Builder.Pages.Insert (Path, Page);
   end Add_Page;


   procedure Add_Page
     (Builder : in out Site_Builder;
      Context : in Meaningless_Type;
      File_Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class)
   is
      pragma Unreferenced (Context);
      use type S_Expressions.Events.Event;
   begin
      if Arguments.Current_Event = S_Expressions.Events.Add_Atom then
         Add_Page (Builder, File_Name, Arguments.Current_Atom);
      end if;
   end Add_Page;


   procedure Add_Page_Simple
     (Builder : in out Site_Builder;
      Context : in Meaningless_Type;
      Common_Name : in S_Expressions.Atom)
   is
      pragma Unreferenced (Context);
   begin
      Add_Page (Builder, Common_Name, Common_Name);
   end Add_Page_Simple;


   procedure Add_Pages is new S_Expressions.Interpreter_Loop
     (Site_Builder, Meaningless_Type, Add_Page, Add_Page_Simple);


   procedure Execute
     (Builder : in out Site_Builder;
      Context : in Meaningless_Type;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class)
   is
      pragma Unreferenced (Context);
      package Commands renames Natools.Static_Maps.Web.Sites;
      use type S_Expressions.Events.Event;
   begin
      case Commands.To_Command (S_Expressions.To_String (Name)) is
         when Commands.Error =>
            Log (Severities.Error, "Unknown site command """
              & S_Expressions.To_String (Name) & '"');

         when Commands.Set_File_Prefix =>
            Set_If_Possible (Builder.File_Prefix, Arguments);

         when Commands.Set_File_Suffix =>
            Set_If_Possible (Builder.File_Suffix, Arguments);

         when Commands.Set_Path_Prefix =>
            Set_If_Possible (Builder.Path_Prefix, Arguments);

         when Commands.Set_Path_Suffix =>
            Set_If_Possible (Builder.Path_Suffix, Arguments);

         when Commands.Site_Map =>
            Add_Pages (Arguments, Builder, Meaningless_Value);
      end case;
   end Execute;


   procedure Interpreter is new S_Expressions.Interpreter_Loop
     (Site_Builder, Meaningless_Type, Execute);


   procedure Update
     (Builder : in out Site_Builder;
      Expression : in out S_Expressions.Lockable.Descriptor'Class) is
   begin
      Interpreter (Expression, Builder, Meaningless_Value);
   end Update;



   ---------------------------
   -- Site Public Interface --
   ---------------------------

   function Create (File_Name : String) return Site is
      Result : Site
        := (File_Name => S_Expressions.Atom_Ref_Constructors.Create
                           (S_Expressions.To_Atom (File_Name)),
            Pages => <>);
   begin
      Reload (Result);
      return Result;
   end Create;


   procedure Reload (Object : in out Site) is
      Reader : S_Expressions.File_Readers.S_Reader
        := S_Expressions.File_Readers.Reader
           (S_Expressions.To_String (Object.File_Name.Query.Data.all));
      Builder : Site_Builder;
   begin
      Update (Builder, Reader);
      Object :=
        (File_Name => Object.File_Name,
         Pages => Page_Maps.Raw_Maps.Create (Builder.Pages));
   end Reload;

end Natools.Web.Sites;
