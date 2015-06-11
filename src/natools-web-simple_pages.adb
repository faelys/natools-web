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
with Natools.S_Expressions.Templates.Dates;
with Natools.Static_Maps.Web.Simple_Pages;
with Natools.Web.Error_Pages;
with Natools.Web.Exchanges;
with Natools.Web.Fallback_Render;

package body Natools.Web.Simple_Pages is

   procedure Append
     (Exchange : in out Sites.Exchange;
      Page : in Page_Data;
      Data : in S_Expressions.Atom);

   procedure Execute
     (Data : in out Page_Data;
      Context : in Meaningless_Type;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class);

   procedure Render
     (Exchange : in out Sites.Exchange;
      Page : in Page_Data;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class);


   procedure Read_Page is new S_Expressions.Interpreter_Loop
     (Page_Data, Meaningless_Type, Execute);

   procedure Render_Page is new S_Expressions.Interpreter_Loop
     (Sites.Exchange, Page_Data, Render, Append);


   ---------------------------
   -- Page Data Constructor --
   ---------------------------

   procedure Execute
     (Data : in out Page_Data;
      Context : in Meaningless_Type;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class)
   is
      pragma Unreferenced (Context);
      package Components renames Natools.Static_Maps.Web.Simple_Pages;
   begin
      case Components.To_Component (S_Expressions.To_String (Name)) is
         when Components.Error =>
            Log (Severities.Error, "Unknown page component """
              & S_Expressions.To_String (Name) & '"');

         when Components.Comment_List =>
            Data.Comment_List.Set (Arguments);

         when Components.Dates =>
            Containers.Set_Dates (Data.Dates, Arguments);

         when Components.Elements =>
            Containers.Set_Expressions (Data.Elements, Arguments);

         when Components.Tags =>
            Tags.Append (Data.Tags, Arguments);
      end case;
   end Execute;



   -------------------
   -- Page Renderer --
   -------------------

   procedure Append
     (Exchange : in out Sites.Exchange;
      Page : in Page_Data;
      Data : in S_Expressions.Atom)
   is
      pragma Unreferenced (Page);
   begin
      Exchange.Append (Data);
   end Append;


   procedure Render
     (Exchange : in out Sites.Exchange;
      Page : in Page_Data;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class)
   is
      use type S_Expressions.Events.Event;

      procedure Re_Enter
        (Exchange : in out Sites.Exchange;
         Expression : in out S_Expressions.Lockable.Descriptor'Class);

      procedure Render_Date (Log_Error : in Boolean);

      procedure Re_Enter
        (Exchange : in out Sites.Exchange;
         Expression : in out S_Expressions.Lockable.Descriptor'Class) is
      begin
         Render_Page (Expression, Exchange, Page);
      end Re_Enter;

      procedure Render_Date (Log_Error : in Boolean) is
      begin
         if Arguments.Current_Event = S_Expressions.Events.Add_Atom then
            declare
               Cursor : constant Containers.Date_Maps.Cursor
                 := Page.Dates.Find (Arguments.Current_Atom);
            begin
               if not Containers.Date_Maps.Has_Element (Cursor) then
                  if Log_Error then
                     Log (Severities.Error, "Unable to find date """
                       & S_Expressions.To_String (Arguments.Current_Atom)
                       & """ in page date map");
                  end if;

                  return;
               end if;

               Arguments.Next;
               S_Expressions.Templates.Dates.Render
                 (Exchange, Arguments, Containers.Date_Maps.Element (Cursor));
            end;
         end if;
      end Render_Date;

      package Commands renames Natools.Static_Maps.Web.Simple_Pages;
   begin
      case Commands.To_Command (S_Expressions.To_String (Name)) is
         when Commands.Unknown_Command =>
            Fallback_Render
              (Exchange, Name, Arguments,
               "simple page",
               Re_Enter'Access, Page.Elements);

         when Commands.Comment_List =>
            Comments.Render (Exchange, Page.Comment_List, Arguments);

         when Commands.Date =>
            Render_Date (True);

         when Commands.My_Tags =>
            if Arguments.Current_Event = S_Expressions.Events.Add_Atom then
               declare
                  Prefix : constant S_Expressions.Atom
                    := Arguments.Current_Atom;
               begin
                  Arguments.Next;
                  Tags.Render
                    (Exchange,
                     Page.Tags,
                     Exchange.Site.Get_Tags,
                     Prefix,
                     Arguments);
               end;
            end if;

         when Commands.If_No_Date =>
            if Arguments.Current_Event = S_Expressions.Events.Add_Atom
              and then not Page.Dates.Contains (Arguments.Current_Atom)
            then
               Arguments.Next;
               Render_Page (Arguments, Exchange, Page);
            end if;

         when Commands.Optional_Date =>
            Render_Date (False);

         when Commands.Path =>
            Exchange.Append (Page.Web_Path.Query);

         when Commands.Tags =>
            Tags.Render
              (Exchange,
               Exchange.Site.Get_Tags,
               Arguments,
               Page.Tags);
      end case;
   end Render;



   -------------------------
   -- Page_Data Interface --
   -------------------------

   not overriding procedure Get_Element
     (Data : in Page_Data;
      Name : in S_Expressions.Atom;
      Element : out S_Expressions.Caches.Cursor;
      Found : out Boolean)
   is
      Cursor : constant Containers.Expression_Maps.Cursor
        := Data.Elements.Find (Name);
   begin
      Found := Containers.Expression_Maps.Has_Element (Cursor);

      if Found then
         Element := Containers.Expression_Maps.Element (Cursor);
      end if;
   end Get_Element;



   ----------------------
   -- Public Interface --
   ----------------------

   function Create
     (File_Path, Web_Path : in S_Expressions.Atom_Refs.Immutable_Reference)
     return Page_Ref
   is
      Page : constant Data_Refs.Data_Access := new Page_Data'
        (File_Path => File_Path,
         Web_Path => Web_Path,
         Tags => <>,
         Self => null,
         Comment_List | Elements | Dates => <>);
      Result : constant Page_Ref := (Ref => Data_Refs.Create (Page));
   begin
      Page.Self := Tags.Visible_Access (Page);

      Create_Page :
      declare
         Reader : Natools.S_Expressions.File_Readers.S_Reader
           := Natools.S_Expressions.File_Readers.Reader
              (S_Expressions.To_String (File_Path.Query));
      begin
         Read_Page (Reader, Page.all, Meaningless_Value);
      end Create_Page;

      return Result;
   end Create;


   overriding procedure Render
     (Exchange : in out Sites.Exchange;
      Object : in Page_Ref;
      Expression : in out S_Expressions.Lockable.Descriptor'Class) is
   begin
      Render_Page
        (Expression,
         Exchange,
         Object.Ref.Query.Data.all);
   end Render;


   overriding procedure Render
     (Exchange : in out Sites.Exchange;
      Object : in Page_Data;
      Expression : in out S_Expressions.Lockable.Descriptor'Class) is
   begin
      Render_Page (Expression, Exchange, Object);
   end Render;


   overriding procedure Respond
     (Object : in out Page_Ref;
      Exchange : in out Sites.Exchange;
      Extra_Path : in S_Expressions.Atom)
   is
      use type S_Expressions.Offset;
   begin
      if Extra_Path'Length = 9
        and then S_Expressions.To_String (Extra_Path) = "/comments"
      then
         Object.Ref.Update.Comment_List.Respond
           (Exchange, Extra_Path (Extra_Path'First + 9 .. Extra_Path'Last));
         return;
      end if;

      if Extra_Path'Length > 0 then
         return;
      end if;

      declare
         Accessor : constant Data_Refs.Accessor := Object.Ref.Query;
         Expression : S_Expressions.Caches.Cursor;
      begin
         Check_Method :
         declare
            use Exchanges;
            Allowed : Boolean;
         begin
            Error_Pages.Check_Method
              (Exchange,
               (GET, HEAD),
               Allowed);

            if not Allowed then
               return;
            end if;
         end Check_Method;

         Expression := Exchange.Site.Get_Template
           (Accessor.Data.Elements,
            Expression,
            Exchange.Site.Default_Template,
            Lookup_Element => True,
            Lookup_Template => True,
            Lookup_Name => True);

         Render_Page (Expression, Exchange, Accessor.Data.all);
      end;
   end Respond;



   -----------------------
   -- Page Constructors --
   -----------------------

   function Create (File : in S_Expressions.Atom)
     return Sites.Page_Loader'Class is
   begin
      return Loader'(File_Path
        => S_Expressions.Atom_Ref_Constructors.Create (File));
   end Create;


   overriding procedure Load
     (Object : in out Loader;
      Builder : in out Sites.Site_Builder;
      Path : in S_Expressions.Atom)
   is
      Page : constant Page_Ref := Create
        (Object.File_Path,
         S_Expressions.Atom_Ref_Constructors.Create (Path));
   begin
      Sites.Insert (Builder, Path, Page);
      Sites.Insert (Builder, Page.Get_Tags, Page);

      declare
         Mutator : constant Data_Refs.Mutator := Page.Ref.Update;
      begin
         Mutator.Comment_List.Load (Builder, Mutator.Self, Mutator.Web_Path);
      end;
   end Load;


   procedure Register_Loader (Site : in out Sites.Site) is
   begin
      Site.Register ("simple-page", Create'Access);
   end Register_Loader;

end Natools.Web.Simple_Pages;
