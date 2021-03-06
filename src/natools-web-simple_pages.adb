------------------------------------------------------------------------------
-- Copyright (c) 2014-2019, Natacha Porté                                   --
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
with Natools.S_Expressions.Enumeration_IO;
with Natools.S_Expressions.File_Readers;
with Natools.S_Expressions.Interpreter_Loop;
with Natools.S_Expressions.Templates.Dates;
with Natools.Static_Maps.Web.Simple_Pages;
with Natools.Web.Error_Pages;
with Natools.Web.Exchanges;
with Natools.Web.Fallback_Render;

package body Natools.Web.Simple_Pages is

   package Template_Components is
      type Enum is
        (Unknown,
         Comment_List,
         Comment_Path_Prefix,
         Comment_Path_Suffix,
         Comments,
         Elements);
      package IO is new Natools.S_Expressions.Enumeration_IO.Typed_IO (Enum);
   end Template_Components;

   Expiration_Date_Key : constant S_Expressions.Atom
     := S_Expressions.To_Atom ("!expire");
   Publication_Date_Key : constant S_Expressions.Atom
     := S_Expressions.To_Atom ("!publish");


   procedure Append
     (Exchange : in out Sites.Exchange;
      Page : in Page_Data;
      Data : in S_Expressions.Atom);

   function Comment_Path_Override
     (Template : in Page_Template;
      Override : in S_Expressions.Lockable.Descriptor'Class)
     return S_Expressions.Atom_Refs.Immutable_Reference;

   procedure Execute
     (Data : in out Page_Data;
      Context : in Page_Template;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class);

   procedure Render
     (Exchange : in out Sites.Exchange;
      Page : in Page_Data;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class);

   procedure Set_Component
     (Object : in out Page_Template;
      Context : in Meaningless_Type;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class);


   procedure Read_Page is new S_Expressions.Interpreter_Loop
     (Page_Data, Page_Template, Execute);

   procedure Render_Page is new S_Expressions.Interpreter_Loop
     (Sites.Exchange, Page_Data, Render, Append);

   procedure Update_Template is new S_Expressions.Interpreter_Loop
     (Page_Template, Meaningless_Type, Set_Component);


   ---------------------------
   -- Page Data Constructor --
   ---------------------------

   procedure Execute
     (Data : in out Page_Data;
      Context : in Page_Template;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class)
   is
      package Components renames Natools.Static_Maps.Web.Simple_Pages;
   begin
      case Components.To_Component (S_Expressions.To_String (Name)) is
         when Components.Error =>
            Log (Severities.Error, "Unknown page component """
              & S_Expressions.To_String (Name) & '"');

         when Components.Comment_List =>
            if Context.Comments.Is_Empty then
               Data.Comment_List.Set (Arguments);
            else
               declare
                  Template : S_Expressions.Caches.Cursor
                    := Context.Comments.Value;
               begin
                  Data.Comment_List.Set
                    (Template,
                     Arguments,
                     Comment_Path_Override (Context, Arguments));
               end;
            end if;

         when Components.Dates =>
            Containers.Set_Dates (Data.Dates, Arguments);

         when Components.Elements =>
            Containers.Add_Expressions (Data.Elements, Arguments);

         when Components.Maps =>
            Data.Maps := String_Tables.Create (Arguments);

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

               declare
                  Item : constant Containers.Date
                    := Containers.Date_Maps.Element (Cursor);
               begin
                  S_Expressions.Templates.Dates.Render
                    (Exchange, Arguments, Item.Time, Item.Offset);
               end;
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

         when Commands.Maps =>
            String_Tables.Render (Exchange, Page.Maps, Arguments);

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



   -----------------------------
   -- Page_Template Interface --
   -----------------------------

   function Comment_Path_Override
     (Template : in Page_Template;
      Override : in S_Expressions.Lockable.Descriptor'Class)
     return S_Expressions.Atom_Refs.Immutable_Reference
   is
      use type S_Expressions.Atom;
      use type S_Expressions.Events.Event;
      Name_Override : constant Boolean
        := Override.Current_Event = S_Expressions.Events.Add_Atom;
      Name : constant S_Expressions.Atom
        := (if Name_Override then Override.Current_Atom
            elsif not Template.Name.Is_Empty then Template.Name.Query
            else S_Expressions.Null_Atom);
   begin
      if Template.Comment_Path_Prefix.Is_Empty
        or else (not Name_Override and then Template.Name.Is_Empty)
      then
         return S_Expressions.Atom_Refs.Null_Immutable_Reference;
      else
         return S_Expressions.Atom_Ref_Constructors.Create
           (Template.Comment_Path_Prefix.Query
            & Name
            & (if Template.Comment_Path_Suffix.Is_Empty
               then S_Expressions.Null_Atom
               else Template.Comment_Path_Suffix.Query));
      end if;
   end Comment_Path_Override;


   procedure Set_Comments
     (Object : in out Page_Template;
      Expression : in out S_Expressions.Lockable.Descriptor'Class) is
   begin
      Object.Comments
        := (Is_Empty => False,
            Value => S_Expressions.Caches.Conditional_Move (Expression));
   end Set_Comments;


   procedure Set_Comment_Path_Prefix
     (Object : in out Page_Template;
      Prefix : in S_Expressions.Atom) is
   begin
      if Prefix'Length > 0 then
         Object.Comment_Path_Prefix
           := S_Expressions.Atom_Ref_Constructors.Create (Prefix);
      else
         Object.Comment_Path_Prefix.Reset;
      end if;
   end Set_Comment_Path_Prefix;


   procedure Set_Comment_Path_Suffix
     (Object : in out Page_Template;
      Suffix : in S_Expressions.Atom) is
   begin
      if Suffix'Length > 0 then
         Object.Comment_Path_Suffix
           := S_Expressions.Atom_Ref_Constructors.Create (Suffix);
      else
         Object.Comment_Path_Suffix.Reset;
      end if;
   end Set_Comment_Path_Suffix;


   procedure Set_Component
     (Object : in out Page_Template;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class;
      Known_Component : out Boolean)
   is
      use type S_Expressions.Events.Event;
   begin
      Known_Component := True;

      case Template_Components.IO.Value (Name, Template_Components.Unknown) is
         when Template_Components.Unknown =>
            Known_Component := False;

         when Template_Components.Comment_List
            | Template_Components.Comments
         =>
            Set_Comments (Object, Arguments);

         when Template_Components.Comment_Path_Prefix =>
            Set_Comment_Path_Prefix
              (Object,
               (if Arguments.Current_Event = S_Expressions.Events.Add_Atom
               then Arguments.Current_Atom
               else S_Expressions.Null_Atom));

         when Template_Components.Comment_Path_Suffix =>
            Set_Comment_Path_Suffix
              (Object,
               (if Arguments.Current_Event = S_Expressions.Events.Add_Atom
               then Arguments.Current_Atom
               else S_Expressions.Null_Atom));

         when Template_Components.Elements =>
            Set_Elements (Object, Arguments);
      end case;
   end Set_Component;


   procedure Set_Component
     (Object : in out Page_Template;
      Context : in Meaningless_Type;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class)
   is
      pragma Unreferenced (Context);
      Known_Component : Boolean;
   begin
      Set_Component (Object, Name, Arguments, Known_Component);

      if not Known_Component then
         Log (Severities.Error, "Unknown simple page template component """
           & S_Expressions.To_String (Name) & '"');
      end if;
   end Set_Component;


   procedure Set_Elements
     (Object : in out Page_Template;
      Expression : in out S_Expressions.Lockable.Descriptor'Class) is
   begin
      Containers.Set_Expressions (Object.Elements, Expression);
   end Set_Elements;


   procedure Update
     (Object : in out Page_Template;
      Expression : in out S_Expressions.Lockable.Descriptor'Class) is
   begin
      Update_Template (Expression, Object, Meaningless_Value);
   end Update;



   ----------------------
   -- Public Interface --
   ----------------------

   function Create
     (Expression : in out S_Expressions.Lockable.Descriptor'Class;
      Template : in Page_Template := Default_Template;
      Name : in S_Expressions.Atom := S_Expressions.Null_Atom)
     return Page_Ref
   is
      Page : constant Data_Refs.Data_Access := new Page_Data;
      Result : constant Page_Ref := (Ref => Data_Refs.Create (Page));
      Actual_Template : Page_Template := Template;
   begin
      Actual_Template.Name
        := (if Name'Length > 0
            then S_Expressions.Atom_Ref_Constructors.Create (Name)
            else S_Expressions.Atom_Refs.Null_Immutable_Reference);
      Page.Self := Tags.Visible_Access (Page);
      Page.Elements := Template.Elements;
      Read_Page (Expression, Page.all, Actual_Template);
      return Result;
   end Create;


   function Create
     (File_Path, Web_Path : in S_Expressions.Atom_Refs.Immutable_Reference;
      Template : in Page_Template := Default_Template;
      Name : in S_Expressions.Atom := S_Expressions.Null_Atom)
     return Page_Ref
   is
      Page : constant Data_Refs.Data_Access := new Page_Data'
        (File_Path => File_Path,
         Web_Path => Web_Path,
         Elements => Template.Elements,
         Tags => <>,
         Self => null,
         Comment_List | Dates | Maps => <>);
      Result : constant Page_Ref := (Ref => Data_Refs.Create (Page));
      Actual_Template : Page_Template := Template;
   begin
      Page.Self := Tags.Visible_Access (Page);
      Actual_Template.Name
        := (if Name'Length > 0
            then S_Expressions.Atom_Ref_Constructors.Create (Name)
            else S_Expressions.Atom_Refs.Null_Immutable_Reference);

      Create_Page :
      declare
         Reader : Natools.S_Expressions.File_Readers.S_Reader
           := Natools.S_Expressions.File_Readers.Reader
              (S_Expressions.To_String (File_Path.Query));
      begin
         Read_Page (Reader, Page.all, Actual_Template);
      end Create_Page;

      return Result;
   end Create;


   procedure Get_Lifetime
     (Page : in Page_Ref;
      Publication : out Ada.Calendar.Time;
      Has_Publication : out Boolean;
      Expiration : out Ada.Calendar.Time;
      Has_Expiration : out Boolean)
   is
      Accessor : constant Data_Refs.Accessor := Page.Ref.Query;
      Cursor : Containers.Date_Maps.Cursor;
   begin
      Cursor := Accessor.Dates.Find (Expiration_Date_Key);

      if Containers.Date_Maps.Has_Element (Cursor) then
         Has_Expiration := True;
         Expiration := Containers.Date_Maps.Element (Cursor).Time;
      else
         Has_Expiration := False;
      end if;

      Cursor := Accessor.Dates.Find (Publication_Date_Key);

      if Containers.Date_Maps.Has_Element (Cursor) then
         Has_Publication := True;
         Publication := Containers.Date_Maps.Element (Cursor).Time;
      else
         Has_Publication := False;
      end if;
   end Get_Lifetime;


   procedure Register
     (Page : in Page_Ref;
      Builder : in out Sites.Site_Builder;
      Path : in S_Expressions.Atom) is
   begin
      Time_Check :
      declare
         use type Ada.Calendar.Time;
         Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
         Publication : Ada.Calendar.Time;
         Has_Publication : Boolean;
         Expiration : Ada.Calendar.Time;
         Has_Expiration : Boolean;
      begin
         Get_Lifetime
           (Page, Publication, Has_Publication, Expiration, Has_Expiration);

         if Has_Publication and then Publication >= Now then
            Sites.Expire_At (Builder, Publication);
            return;
         end if;

         if Has_Expiration then
            if Expiration < Now then
               return;
            else
               Sites.Expire_At (Builder, Expiration);
            end if;
         end if;
      end Time_Check;

      if Path'Length > 0 then
         Sites.Insert (Builder, Path, Page);
      end if;

      Sites.Insert (Builder, Page.Get_Tags, Page);

      Load_Comments :
      declare
         Mutator : constant Data_Refs.Mutator := Page.Ref.Update;
      begin
         Mutator.Comment_List.Load (Builder, Mutator.Self, Mutator.Web_Path);
      end Load_Comments;
   end Register;


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
      Register (Page, Builder, Path);
   end Load;


   procedure Register_Loader (Site : in out Sites.Site) is
   begin
      Site.Register ("simple-page", Create'Access);
   end Register_Loader;

end Natools.Web.Simple_Pages;
