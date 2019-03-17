------------------------------------------------------------------------------
-- Copyright (c) 2015-2019, Natacha Porté                                   --
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

------------------------------------------------------------------------------
-- Natools.Web.Comments provides an implementation of user-posted comments  --
-- like is commonly found on blogs.                                         --
------------------------------------------------------------------------------

with Ada.Characters.Handling;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Exceptions;
with Ada.Streams.Stream_IO;
with Natools.File_Streams;
with Natools.S_Expressions.Atom_Buffers;
with Natools.S_Expressions.Atom_Ref_Constructors;
with Natools.S_Expressions.Caches;
with Natools.S_Expressions.Conditionals.Generic_Evaluate;
with Natools.S_Expressions.Conditionals.Strings;
with Natools.S_Expressions.Enumeration_IO;
with Natools.S_Expressions.Interpreter_Loop;
with Natools.S_Expressions.Parsers;
with Natools.S_Expressions.Printers.Pretty;
with Natools.S_Expressions.Templates.Dates;
with Natools.S_Expressions.Templates.Integers;
with Natools.Static_Maps.Web.Comments;
with Natools.Time_IO.RFC_3339;
with Natools.Time_Keys;
with Natools.Web.Backends;
with Natools.Web.Comment_Cookies;
with Natools.Web.Error_Pages;
with Natools.Web.Escapes;
with Natools.Web.Exchanges;
with Natools.Web.Fallback_Render;
with Natools.Web.Filters.Stores;
with Natools.Web.Filters.Text_Blocks;
with Natools.Web.Is_Valid_URL;
with Natools.Web.List_Templates;
with Natools.Web.Render_Default;

package body Natools.Web.Comments is

   package Static_Maps renames Natools.Static_Maps.Web.Comments;

   package Comment_Flag_IO is new S_Expressions.Enumeration_IO.Typed_IO
     (Comment_Flags.Enum);

   package List_Flag_IO is new S_Expressions.Enumeration_IO.Typed_IO
     (List_Flags.Enum);

   package String_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (String, String);


   type Post_Action is (Save_Comment, Force_Preview, Parent_Redirect);

   package Post_Action_IO is new S_Expressions.Enumeration_IO.Typed_IO
     (Post_Action);

   type Comment_Builder is record
      Core : Comment_Data;
      Extra_Fields, Raw_Fields : String_Maps.Map;
      Has_Unknown_Field : Boolean := False;
      Cookie_Save : Boolean := False;
      Action : Post_Action;
      Reason : S_Expressions.Atom_Refs.Immutable_Reference;
      Anchor : S_Expressions.Atom_Refs.Immutable_Reference;
      Redirect_Base : S_Expressions.Atom_Refs.Immutable_Reference;
   end record;


   Invalid_Condition : exception;


   Ignore_Button : constant String := "ignore";
   Preview_Button : constant String := "preview";
   Preview_Id_Ref : S_Expressions.Atom_Refs.Immutable_Reference;
   Submit_Button : constant String := "submit";


   procedure Append
     (Exchange : in out Sites.Exchange;
      Context : in Comment_List;
      Data : in S_Expressions.Atom);
      --  Append Data to Exchange, ignoring Context

   procedure Append
     (Exchange : in out Sites.Exchange;
      Context : in Comment_Data;
      Data : in S_Expressions.Atom);
      --  Append Data to Exchange, ignoring Context

   function Create (Data : S_Expressions.Atom)
     return S_Expressions.Atom_Refs.Immutable_Reference
     renames S_Expressions.Atom_Ref_Constructors.Create;
      --  Atom expression constructor

   function Evaluate_Parametric
     (Builder : in Comment_Builder;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class)
     return Boolean;
      --  Evaluate a condition on a comment builder with arguments

   function Evaluate_Simple
     (Builder : in Comment_Builder;
      Name : in S_Expressions.Atom)
     return Boolean;
      --  Evaluate a condition on a comment builder without argument

   function Get_Safe_Filter
     (Site : in Sites.Site;
      Name_Ref : in S_Expressions.Atom_Refs.Immutable_Reference;
      Default : in S_Expressions.Atom_Refs.Immutable_Reference)
     return Filters.Filter'Class;
   function Get_Safe_Filter
     (Builder : in Sites.Site_Builder;
      Name_Ref : in S_Expressions.Atom_Refs.Immutable_Reference;
      Default : in S_Expressions.Atom_Refs.Immutable_Reference)
     return Filters.Filter'Class;
      --  Return the filter designated by Name_Ref, falling back on raw
      --  text when Name_Ref is empty or named filter is not found.

   function Image (Name : Comment_Atoms.Enum) return String;
      --  Return a string representation of Name

   function Next_Rank (List : Comment_List) return Positive;
      --  Return the next rank for a comment in List (i.e. length + 1)

   procedure Parse_Action
     (Builder : in out Comment_Builder;
      Site : in Sites.Site;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class);
      --  Evluate action with parameter

   procedure Parse_Action_Simple
     (Builder : in out Comment_Builder;
      Site : in Sites.Site;
      Name : in S_Expressions.Atom);
      --  Evaluate parameterless action

   procedure Process_Actions
     (Builder : in out Comment_Builder;
      Site : in Sites.Site;
      Filter_Name : in S_Expressions.Atom_Refs.Immutable_Reference);
      --  Evaluate the filter whose name is given

   procedure Preprocess
     (Comment : in out Comment_Data;
      Text_Filter : in Filters.Filter'Class);
      --  Common preprocessing code, after Text_Filter is determined

   function Preview_Id return S_Expressions.Atom_Refs.Immutable_Reference;
      --  Return the comment id of temporary previews

   procedure Process_Form
     (Data : in out Comment_Builder;
      Exchange : in Sites.Exchange;
      List : in Comment_List);
      --  Read form data in Exchange to fill Data

   procedure Render
     (Exchange : in out Sites.Exchange;
      Position : in Comment_Maps.Cursor;
      Expression : in out S_Expressions.Lockable.Descriptor'Class);

   procedure Render_Comment
     (Exchange : in out Sites.Exchange;
      Comment : in Comment_Data;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class);

   procedure Render_List_Element
     (Exchange : in out Sites.Exchange;
      List : in Comment_List;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class);
      --  Render a command

   procedure Reset_If_Blank
     (Ref : in out S_Expressions.Atom_Refs.Immutable_Reference);
      --  Reset Ref if it only contains blank characters

   function String_Fallback_Parametric
     (Settings : in S_Expressions.Conditionals.Strings.Settings;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class)
     return Boolean;
      --  Raise Invalid_Condition

   function String_Fallback_Simple
     (Settings : in S_Expressions.Conditionals.Strings.Settings;
      Name : in S_Expressions.Atom)
     return Boolean;
      --  Raise Invalid_Condition

   procedure Update_Item
     (Comment : in out Comment_Data;
      Context : in Meaningless_Type;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class);
      --  Update comment with the given expression

   procedure Update_List
     (List : in out Comment_List;
      Context : in Meaningless_Type;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class);
      --  Update comment list with the given expression

   function Value (Name : S_Expressions.Atom) return Comment_Atoms.Enum;
      --  Convert Name into a comment atom reference, raising Constraint_Error
      --  when Name is not valid.

   procedure Write
     (Builder : in Comment_Builder;
      Output : in out S_Expressions.Printers.Printer'Class);
      --  Serialize a builder into the given S_Expression stream

   procedure Write
     (Builder : in Comment_Builder;
      Site : in Sites.Site;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class);
      --  Serialize a builder into the file named by Arguments.Current_Atom.
      --  The Site argument is useless for now, but kept for a future
      --  backend-based implementation.


   function Evaluate is new S_Expressions.Conditionals.Generic_Evaluate
     (Comment_Builder, Evaluate_Parametric, Evaluate_Simple);

   procedure Parse is new S_Expressions.Interpreter_Loop
     (Comment_Builder, Sites.Site, Parse_Action, Parse_Action_Simple);

   procedure Render is new S_Expressions.Interpreter_Loop
     (Sites.Exchange, Comment_List, Render_List_Element, Append);

   procedure Render is new S_Expressions.Interpreter_Loop
     (Sites.Exchange, Comment_Data, Render_Comment, Append);

   procedure Render_List is new List_Templates.Render
     (Comment_Maps.Cursor, Comment_Maps.Map_Iterator_Interfaces);

   procedure Update is new S_Expressions.Interpreter_Loop
     (Comment_Data, Meaningless_Type, Update_Item);

   procedure Update is new S_Expressions.Interpreter_Loop
     (Comment_List, Meaningless_Type, Update_List);



   ------------------------------
   -- Local Helper Subprograms --
   ------------------------------

   function Image (Name : Comment_Atoms.Enum) return String is
   begin
      return Ada.Characters.Handling.To_Lower
        (Comment_Atoms.Enum'Image (Name));
   end Image;


   function Next_Rank (List : Comment_List) return Positive is
   begin
      return List.Comments.Query.Length + 1;
   end Next_Rank;


   function Value (Name : S_Expressions.Atom) return Comment_Atoms.Enum is
   begin
      return Comment_Atoms.Enum'Value (S_Expressions.To_String (Name));
   end Value;



   ---------------
   -- Renderers --
   ---------------

   procedure Append
     (Exchange : in out Sites.Exchange;
      Context : in Comment_List;
      Data : in S_Expressions.Atom)
   is
      pragma Unreferenced (Context);
   begin
      Exchange.Append (Data);
   end Append;


   procedure Append
     (Exchange : in out Sites.Exchange;
      Context : in Comment_Data;
      Data : in S_Expressions.Atom)
   is
      pragma Unreferenced (Context);
   begin
      Exchange.Append (Data);
   end Append;


   procedure Render
     (Exchange : in out Sites.Exchange;
      Position : in Comment_Maps.Cursor;
      Expression : in out S_Expressions.Lockable.Descriptor'Class) is
   begin
      Render (Expression, Exchange, Comment_Maps.Element (Position));
   end Render;


   procedure Render_Comment
     (Exchange : in out Sites.Exchange;
      Comment : in Comment_Data;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class)
   is
      use type Tags.Visible_Access;
      use Static_Maps.Item.Command;

      procedure Re_Enter
        (Exchange : in out Sites.Exchange;
         Expression : in out S_Expressions.Lockable.Descriptor'Class);

      procedure Render_Ref
        (Ref : in S_Expressions.Atom_Refs.Immutable_Reference);

      procedure Value
        (Image : in String;
         Found : out Boolean;
         Id : out Comment_Atoms.Enum);

      procedure Re_Enter
        (Exchange : in out Sites.Exchange;
         Expression : in out S_Expressions.Lockable.Descriptor'Class) is
      begin
         Render (Expression, Exchange, Comment);
      end Re_Enter;

      procedure Render_Ref
        (Ref : in S_Expressions.Atom_Refs.Immutable_Reference) is
      begin
         if not Ref.Is_Empty then
            Exchange.Append (Ref.Query);
         elsif Arguments.Current_Event
           in S_Expressions.Events.Add_Atom | S_Expressions.Events.Open_List
         then
            Render (Arguments, Exchange, Comment);
         end if;
      end Render_Ref;

      procedure Value
        (Image : in String;
         Found : out Boolean;
         Id : out Comment_Atoms.Enum) is
      begin
         Id := Comment_Atoms.Enum'Value (Image);
         Found := True;
      exception
         when Constraint_Error =>
            Found := False;
      end Value;

      S_Name : constant String := S_Expressions.To_String (Name);
   begin
      pragma Assert (Comment.Flags (Comment_Flags.Preprocessed));

      case Static_Maps.To_Item_Command (S_Name) is
         when Unknown =>
            declare
               Has_Id : Boolean;
               Atom_Id : Comment_Atoms.Enum;
            begin
               if S_Name'Length > 6
                 and then S_Name (S_Name'First .. S_Name'First + 5) = "if-no-"
               then
                  Value
                    (S_Name (S_Name'First + 6 .. S_Name'Last),
                     Has_Id, Atom_Id);

                  if Has_Id and then Comment.Atoms (Atom_Id).Is_Empty then
                     Render (Arguments, Exchange, Comment);
                  end if;

               elsif S_Name'Length > 3
                 and then S_Name (S_Name'First .. S_Name'First + 2) = "if-"
               then
                  Value
                    (S_Name (S_Name'First + 3 .. S_Name'Last),
                     Has_Id, Atom_Id);

                  if Has_Id and then not Comment.Atoms (Atom_Id).Is_Empty then
                     Render (Arguments, Exchange, Comment);
                  end if;

               elsif S_Name /= "filter" then
                  Value (S_Name, Has_Id, Atom_Id);

                  if Has_Id then
                     Render_Ref (Comment.Atoms (Atom_Id));
                  end if;
               end if;

               if not Has_Id then
                  Fallback_Render
                    (Exchange, Name, Arguments, "comment", Re_Enter'Access);
               end if;
            end;

         when Date =>
            S_Expressions.Templates.Dates.Render
              (Exchange, Arguments, Comment.Date, Comment.Offset);

         when Id =>
            Exchange.Append (Comment.Id.Query);

         when Parent =>
            if Comment.Parent /= null then
               Tags.Render (Exchange, Comment.Parent.all, Arguments);
            end if;

         when Rank =>
            S_Expressions.Templates.Integers.Render
              (Exchange, Arguments, Comment.Rank);
      end case;
   end Render_Comment;


   procedure Render_List_Element
     (Exchange : in out Sites.Exchange;
      List : in Comment_List;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class)
   is
      use Static_Maps.List.Command;

      procedure Re_Enter
        (Exchange : in out Sites.Exchange;
         Expression : in out S_Expressions.Lockable.Descriptor'Class);

      procedure Re_Enter
        (Exchange : in out Sites.Exchange;
         Expression : in out S_Expressions.Lockable.Descriptor'Class) is
      begin
         Render (Expression, Exchange, List);
      end Re_Enter;
   begin
      case Static_Maps.To_List_Command (S_Expressions.To_String (Name)) is
         when Unknown =>
            Fallback_Render
              (Exchange, Name, Arguments, "comment list", Re_Enter'Access);

         when If_Closed =>
            if List.Flags (List_Flags.Closed) then
               Render (Arguments, Exchange, List);
            end if;

         when If_Not_Closed =>
            if not List.Flags (List_Flags.Closed) then
               Render (Arguments, Exchange, List);
            end if;

         when Static_Maps.List.Command.List =>
            Render_List
              (Exchange,
               List.Comments.Query.Iterate,
               List_Templates.Read_Parameters (Arguments));

         when Parent =>
            if not Tags."=" (List.Parent, null) then
               Tags.Render (Exchange, List.Parent.all, Arguments);
            end if;

         when Preview =>
            if Exchange.Parameter (Preview_Button) = ""
              and then Exchange.Parameter (Submit_Button) = ""
            then
               return;
            end if;

            declare
               Builder : Comment_Builder;
            begin
               Builder.Core.Date := Ada.Calendar.Clock;
               Builder.Core.Offset := Ada.Calendar.Time_Zones.UTC_Time_Offset
                 (Builder.Core.Date);
               Builder.Core.Id := Preview_Id;
               Builder.Core.Parent := List.Parent;
               Builder.Core.Rank := Next_Rank (List);
               Process_Form (Builder, Exchange, List);
               Preprocess (Builder.Core, List, Exchange.Site.all);
               Render (Arguments, Exchange, Builder.Core);
            end;

         when Size =>
            S_Expressions.Templates.Integers.Render
              (Exchange, Arguments, List.Comments.Query.Length);
      end case;
   end Render_List_Element;



   ------------------
   -- Interpreters --
   ------------------

   procedure Update_Item
     (Comment : in out Comment_Data;
      Context : in Meaningless_Type;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class)
   is
      pragma Unreferenced (Context);
      use Static_Maps.Item.Element;
      use type S_Expressions.Events.Event;
   begin
      pragma Assert (not Comment.Flags (Comment_Flags.Preprocessed));

      if Arguments.Current_Event /= S_Expressions.Events.Add_Atom then
         return;
      end if;

      case Static_Maps.To_Item_Element (S_Expressions.To_String (Name)) is
         when Unknown =>
            Set_Atom :
            begin
               Comment.Atoms (Value (Name)) := Create (Arguments.Current_Atom);
            exception
               when Constraint_Error =>
                  Log (Severities.Error,
                    "Unknown comment element """
                     & S_Expressions.To_String (Name) & '"');
            end Set_Atom;

         when Date =>
            declare
               Image : constant String
                 := S_Expressions.To_String (Arguments.Current_Atom);
            begin
               Time_IO.RFC_3339.Value (Image, Comment.Date, Comment.Offset);
            exception
               when others =>
                  Log (Severities.Error, "Invalid date """
                    & Image & """ for comment "
                    & S_Expressions.To_String (Comment.Id.Query));
            end;

         when Flags =>
            while Arguments.Current_Event in S_Expressions.Events.Add_Atom loop
               begin
                  Comment.Flags
                    (Comment_Flag_IO.Value (Arguments.Current_Atom))
                    := True;
               exception
                  when Constraint_Error =>
                     Log (Severities.Error, "Invalid comment flag value """
                       & S_Expressions.To_String (Arguments.Current_Atom)
                       & '"');
               end;

               Arguments.Next;
            end loop;
      end case;
   end Update_Item;


   procedure Update_List
     (List : in out Comment_List;
      Context : in Meaningless_Type;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class)
   is
      pragma Unreferenced (Context);
      use Static_Maps.List.Element;
      use type S_Expressions.Events.Event;
      Event : S_Expressions.Events.Event;
   begin
      if Arguments.Current_Event /= S_Expressions.Events.Add_Atom then
         return;
      end if;

      case Static_Maps.To_List_Element (S_Expressions.To_String (Name)) is
         when Unknown =>
            null;

         when Backend =>
            List.Backend_Name := Create (Arguments.Current_Atom);
            Arguments.Next (Event);

            if Event = S_Expressions.Events.Add_Atom then
               List.Backend_Path := Create (Arguments.Current_Atom);
            end if;

         when Default_Text_Filter =>
            List.Default_Text_Filter := Create (Arguments.Current_Atom);

         when Flags =>
            while Arguments.Current_Event in S_Expressions.Events.Add_Atom loop
               begin
                  List.Flags
                    (List_Flag_IO.Value (Arguments.Current_Atom))
                    := True;
               exception
                  when Constraint_Error =>
                     Log (Severities.Error, "Invalid comment list flag """
                       & S_Expressions.To_String (Arguments.Current_Atom)
                       & '"');
               end;

               Arguments.Next;
            end loop;

         when Post_Filter =>
            List.Post_Filter := Create (Arguments.Current_Atom);

         when Static_Maps.List.Element.Tags =>
            declare
               List_Builder : Containers.Unsafe_Atom_Lists.List;
            begin
               Containers.Append_Atoms (List_Builder, Arguments);
               List.Tags := Containers.Create (List_Builder);
            end;

         when Text_Filters =>
            declare
               List_Builder : Containers.Unsafe_Atom_Lists.List;
            begin
               Containers.Append_Atoms (List_Builder, Arguments);
               List.Text_Filters := Containers.Create (List_Builder);
            end;
      end case;
   end Update_List;



   --------------------------------
   -- Conditionals and Filtering --
   --------------------------------

   function Evaluate_Parametric
     (Builder : in Comment_Builder;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class)
     return Boolean
   is
      function Dereference
        (Ref : in S_Expressions.Atom_Refs.Immutable_Reference)
        return String;

      function String_Evaluate
        (Ref : in S_Expressions.Atom_Refs.Immutable_Reference;
         Arg : in out S_Expressions.Lockable.Descriptor'Class)
        return Boolean;

      function Dereference
        (Ref : in S_Expressions.Atom_Refs.Immutable_Reference)
        return String is
      begin
         if Ref.Is_Empty then
            return "";
         else
            return S_Expressions.To_String (Ref.Query);
         end if;
      end Dereference;

      function String_Evaluate
        (Ref : in S_Expressions.Atom_Refs.Immutable_Reference;
         Arg : in out S_Expressions.Lockable.Descriptor'Class)
        return Boolean
      is
         Value : aliased constant String := Dereference (Ref);
         Context : constant S_Expressions.Conditionals.Strings.Context
           := (Data => Value'Access,
               Parametric_Fallback => String_Fallback_Parametric'Access,
               Simple_Fallback => String_Fallback_Simple'Access,
               Settings => <>);
      begin
         return S_Expressions.Conditionals.Strings.Evaluate (Context, Arg);
      end String_Evaluate;

      use Static_Maps.Item.Condition;
      use type S_Expressions.Events.Event;
   begin
      case Static_Maps.To_Item_Condition (S_Expressions.To_String (Name)) is
         when Unknown =>
            Evaluate_Atom :
            begin
               return String_Evaluate
                 (Builder.Core.Atoms (Value (Name)), Arguments);
            exception
               when Constraint_Error => null;
            end Evaluate_Atom;

            raise Invalid_Condition with "Unknown parametric conditional """
              & S_Expressions.To_String (Name) & '"';

         when Action_Is =>
            declare
               Action : Post_Action;
               Event : S_Expressions.Events.Event := Arguments.Current_Event;
            begin
               while Event = S_Expressions.Events.Add_Atom loop
                  begin
                     Action := Post_Action_IO.Value (Arguments.Current_Atom);

                     if Action = Builder.Action then
                        return True;
                     end if;
                  exception
                     when Constraint_Error =>
                        Log (Severities.Error, "Invalid post action string """
                          & S_Expressions.To_String (Arguments.Current_Atom)
                          & '"');
                  end;

                  Arguments.Next (Event);
               end loop;

               return False;
            end;

         when Field_List_Is =>
            declare
               Cursor : String_Maps.Cursor := Builder.Raw_Fields.First;
               Event : S_Expressions.Events.Event := Arguments.Current_Event;
            begin
               loop
                  if not String_Maps.Has_Element (Cursor)
                    or else Event /= S_Expressions.Events.Add_Atom
                  then
                     return String_Maps.Has_Element (Cursor)
                       = (Event = S_Expressions.Events.Add_Atom);
                  end if;

                  if String_Maps.Key (Cursor)
                    /= S_Expressions.To_String (Arguments.Current_Atom)
                  then
                     return False;
                  end if;

                  Arguments.Next (Event);
                  String_Maps.Next (Cursor);
               end loop;
            end;

         when Field_List_Contains =>
            declare
               Event : S_Expressions.Events.Event := Arguments.Current_Event;
            begin
               while Event = S_Expressions.Events.Add_Atom loop
                  if not Builder.Raw_Fields.Contains
                    (S_Expressions.To_String (Arguments.Current_Atom))
                  then
                     return False;
                  end if;

                  Arguments.Next (Event);
               end loop;

               return True;
            end;

         when Field_List_Among =>
            declare
               Cursor : String_Maps.Cursor := Builder.Raw_Fields.First;
               Event : S_Expressions.Events.Event := Arguments.Current_Event;
            begin
               while String_Maps.Has_Element (Cursor) loop
                  if Event /= S_Expressions.Events.Add_Atom then
                     return False;
                  end if;

                  if String_Maps.Key (Cursor)
                    = S_Expressions.To_String (Arguments.Current_Atom)
                  then
                     String_Maps.Next (Cursor);
                  end if;

                  Arguments.Next (Event);
               end loop;

               return True;
            end;

         when Fields_Equal =>
            if Arguments.Current_Event /= S_Expressions.Events.Add_Atom then
               return True;
            end if;

            declare
               function Current_Field return String;

               function Current_Field return String is
                  Cursor : constant String_Maps.Cursor
                    := Builder.Raw_Fields.Find
                       (S_Expressions.To_String (Arguments.Current_Atom));
               begin
                  if String_Maps.Has_Element (Cursor) then
                     return String_Maps.Element (Cursor);
                  else
                     return "";
                  end if;
               end Current_Field;

               Reference : constant String := Current_Field;
               Event : S_Expressions.Events.Event;
            begin
               loop
                  Arguments.Next (Event);
                  exit when Event /= S_Expressions.Events.Add_Atom;

                  if Current_Field /= Reference then
                     return False;
                  end if;
               end loop;

               return True;
            end;

         when Has_Extra_Fields =>
            return not Builder.Extra_Fields.Is_Empty;

         when Has_Unknown_Field =>
            return Builder.Has_Unknown_Field;
      end case;
   end Evaluate_Parametric;


   function Evaluate_Simple
     (Builder : in Comment_Builder;
      Name : in S_Expressions.Atom)
     return Boolean
   is
      use Static_Maps.Item.Condition;
   begin
      case Static_Maps.To_Item_Condition (S_Expressions.To_String (Name)) is
         when Unknown
           | Action_Is | Field_List_Is | Field_List_Contains | Field_List_Among
           | Fields_Equal
         =>
            Evaluate_Atom :
            begin
               return not Builder.Core.Atoms (Value (Name)).Is_Empty;
            exception
               when Constraint_Error => null;
            end Evaluate_Atom;

            raise Invalid_Condition with "Unknown simple conditional """
              & S_Expressions.To_String (Name) & '"';

         when Has_Extra_Fields =>
            return not Builder.Extra_Fields.Is_Empty;

         when Has_Unknown_Field =>
            return Builder.Has_Unknown_Field;
      end case;
   end Evaluate_Simple;


   procedure Parse_Action
     (Builder : in out Comment_Builder;
      Site : in Sites.Site;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class)
   is
      use type S_Expressions.Atom;
      use type S_Expressions.Events.Event;

      procedure Append
        (Ref : in out S_Expressions.Atom_Refs.Immutable_Reference;
         Separator : in S_Expressions.Atom;
         Data : in S_Expressions.Atom);
      procedure Update_Reason;

      procedure Append
        (Ref : in out S_Expressions.Atom_Refs.Immutable_Reference;
         Separator : in S_Expressions.Atom;
         Data : in S_Expressions.Atom) is
      begin
         if Ref.Is_Empty then
            Ref := Create (Data);
         else
            Ref := Create (Ref.Query & Separator & Data);
         end if;
      end Append;

      procedure Update_Reason is
         use type S_Expressions.Octet;
         use type S_Expressions.Offset;
      begin
         if Arguments.Current_Event /= S_Expressions.Events.Add_Atom then
            return;
         end if;

         declare
            Text : constant S_Expressions.Atom := Arguments.Current_Atom;
            Event : S_Expressions.Events.Event;
            O : S_Expressions.Offset;
         begin
            Arguments.Next (Event);

            if Event = S_Expressions.Events.Add_Atom then
               Append (Builder.Reason, Text, Arguments.Current_Atom);
            else
               if Text'Length > 1
                 and then Text (Text'First) = Character'Pos ('+')
               then
                  case Text (Text'First + 1) is
                     when Character'Pos ('\') =>
                        Append
                          (Builder.Reason,
                           S_Expressions.Null_Atom,
                           Text (Text'First + 2 .. Text'Last));

                     when Character'Pos ('(') =>
                        O := Text'First + 2;
                        while O in Text'Range
                          and then Text (O) /= Character'Pos (')')
                        loop
                           O := O + 1;
                        end loop;

                        if O in Text'Range then
                           Append
                             (Builder.Reason,
                              Text (Text'First + 2 .. O - 1),
                              Text (O + 1 .. Text'Last));
                        else
                           Append
                             (Builder.Reason,
                              S_Expressions.Null_Atom,
                              Text (Text'First + 1 .. Text'Last));
                        end if;

                     when others =>
                        Append
                          (Builder.Reason,
                           S_Expressions.Null_Atom,
                           Text (Text'First + 1 .. Text'Last));
                  end case;
               else
                  Builder.Reason := Create (Text);
               end if;
            end if;
         end;
      end Update_Reason;

      use Static_Maps.Item.Post_Action;
      S_Name : constant String := S_Expressions.To_String (Name);
   begin
      if S_Name = "if"
        and then Arguments.Current_Event
           in S_Expressions.Events.Add_Atom | S_Expressions.Events.Open_List
      then
         begin
            if Evaluate (Builder, Arguments) then
               Arguments.Next;
               Parse (Arguments, Builder, Site);
            end if;
         exception
            when Ex : Invalid_Condition =>
               Log (Severities.Error, "Invalid comment condition: "
                 & Ada.Exceptions.Exception_Message (Ex));
         end;

         return;
      end if;

      case Static_Maps.To_Item_Action (S_Name) is
         when Unknown =>
            Log (Severities.Error, "Unknown comment action """ & S_Name & '"');

         when Anchor =>
            if Arguments.Current_Event = S_Expressions.Events.Add_Atom then
               Builder.Anchor := Create (Arguments.Current_Atom);
            else
               Builder.Anchor.Reset;
            end if;

         when Append_Reason =>
            if Arguments.Current_Event = S_Expressions.Events.Add_Atom then
               declare
                  First_Part : constant S_Expressions.Atom
                    := Arguments.Current_Atom;
                  Event : S_Expressions.Events.Event;
               begin
                  Arguments.Next (Event);

                  if Event = S_Expressions.Events.Add_Atom then
                     Append
                       (Builder.Reason, First_Part, Arguments.Current_Atom);
                  else
                     Append
                       (Builder.Reason, S_Expressions.Null_Atom, First_Part);
                  end if;
               end;
            end if;

         when Dump =>
            Write (Builder, Site, Arguments);

         when Force_Preview =>
            Builder.Action := Force_Preview;
            Update_Reason;

         when Force_Redirect =>
            if Arguments.Current_Event = S_Expressions.Events.Add_Atom then
               Builder.Redirect_Base := Create (Arguments.Current_Atom);
            else
               Builder.Redirect_Base.Reset;
            end if;

         when Ignore =>
            Builder.Core.Flags (Comment_Flags.Ignored) := True;
            Update_Reason;

         when Reason =>
            Update_Reason;

         when Reject =>
            Builder.Action := Parent_Redirect;
            Update_Reason;

         when Save =>
            Builder.Action := Save_Comment;
            Update_Reason;

         when Set_Reason =>
            if Arguments.Current_Event = S_Expressions.Events.Add_Atom then
               Builder.Reason := Create (Arguments.Current_Atom);
            end if;

         when Unignore =>
            Builder.Core.Flags (Comment_Flags.Ignored) := False;
            Update_Reason;
      end case;
   end Parse_Action;


   procedure Parse_Action_Simple
     (Builder : in out Comment_Builder;
      Site : in Sites.Site;
      Name : in S_Expressions.Atom)
   is
      pragma Unreferenced (Site);
      use Static_Maps.Item.Post_Action;
      S_Name : constant String := S_Expressions.To_String (Name);
   begin
      case Static_Maps.To_Item_Action (S_Name) is
         when Unknown | Dump =>
            Log (Severities.Error, "Unknown comment action """ & S_Name & '"');

         when Anchor =>
            Builder.Anchor.Reset;

         when Append_Reason =>
            null;

         when Force_Preview =>
            Builder.Action := Force_Preview;

         when Force_Redirect =>
            Builder.Redirect_Base.Reset;

         when Ignore =>
            Builder.Core.Flags (Comment_Flags.Ignored) := True;

         when Reason | Set_Reason =>
            Builder.Reason.Reset;

         when Reject =>
            Builder.Action := Parent_Redirect;

         when Save =>
            Builder.Action := Save_Comment;

         when Unignore =>
            Builder.Core.Flags (Comment_Flags.Ignored) := False;
      end case;
   end Parse_Action_Simple;


   procedure Process_Actions
     (Builder : in out Comment_Builder;
      Site : in Sites.Site;
      Filter_Name : in S_Expressions.Atom_Refs.Immutable_Reference)
   is
      Expression : S_Expressions.Caches.Cursor;
      Found : Boolean;
   begin
      if Filter_Name.Is_Empty then
         return;
      end if;

      Site.Get_Template (Filter_Name.Query, Expression, Found);

      if not Found then
         return;
      end if;

      Parse (Expression, Builder, Site);
   end Process_Actions;


   procedure Reset_If_Blank
     (Ref : in out S_Expressions.Atom_Refs.Immutable_Reference) is
   begin
      if Ref.Is_Empty then
         return;
      end if;

      Abort_If_Not_Blank :
      declare
         Accessor : constant S_Expressions.Atom_Refs.Accessor := Ref.Query;
      begin
         for I in Accessor.Data.all'Range loop
            if Accessor.Data (I) not in 9 | 10 | 13 | 32 then
               return;
            end if;
         end loop;
      end Abort_If_Not_Blank;

      Ref.Reset;
   end Reset_If_Blank;


   function String_Fallback_Parametric
     (Settings : in S_Expressions.Conditionals.Strings.Settings;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class)
     return Boolean
   is
      pragma Unreferenced (Settings, Arguments);
   begin
      raise Invalid_Condition with "Unknown string parametric conditional """
        & S_Expressions.To_String (Name) & '"';
      return False;
   end String_Fallback_Parametric;


   function String_Fallback_Simple
     (Settings : in S_Expressions.Conditionals.Strings.Settings;
      Name : in S_Expressions.Atom)
     return Boolean
   is
      pragma Unreferenced (Settings);
   begin
      raise Invalid_Condition with "Unknown string simple conditional """
        & S_Expressions.To_String (Name) & '"';
      return False;
   end String_Fallback_Simple;



   ------------------------
   -- Comment Suprograms --
   ------------------------

   function Get_Safe_Filter
     (Site : in Sites.Site;
      Name_Ref : in S_Expressions.Atom_Refs.Immutable_Reference;
      Default : in S_Expressions.Atom_Refs.Immutable_Reference)
     return Filters.Filter'Class is
   begin
      if not Name_Ref.Is_Empty then
         begin
            return Site.Get_Filter (Name_Ref.Query);
         exception
            when Filters.Stores.No_Filter => null;
         end;
      end if;

      if not Default.Is_Empty then
         begin
            return Site.Get_Filter (Default.Query);
         exception
            when Filters.Stores.No_Filter => null;
         end;
      end if;

      declare
         Fallback : Filters.Text_Blocks.Filter;
      begin
         return Fallback;
      end;
   end Get_Safe_Filter;


   function Get_Safe_Filter
     (Builder : in Sites.Site_Builder;
      Name_Ref : in S_Expressions.Atom_Refs.Immutable_Reference;
      Default : in S_Expressions.Atom_Refs.Immutable_Reference)
     return Filters.Filter'Class is
   begin
      if not Name_Ref.Is_Empty then
         begin
            return Sites.Get_Filter (Builder, Name_Ref.Query);
         exception
            when Filters.Stores.No_Filter => null;
         end;
      end if;

      if not Default.Is_Empty then
         begin
            return Sites.Get_Filter (Builder, Default.Query);
         exception
            when Filters.Stores.No_Filter => null;
         end;
      end if;

      declare
         Fallback : Filters.Text_Blocks.Filter;
      begin
         return Fallback;
      end;
   end Get_Safe_Filter;


   procedure Preprocess
     (Comment : in out Comment_Data;
      List : in Comment_List;
      Site : in Sites.Site)
   is
      use Comment_Atoms;
   begin
      Preprocess
        (Comment,
         Get_Safe_Filter
           (Site, Comment.Atoms (Filter), List.Default_Text_Filter));
   end Preprocess;


   procedure Preprocess
     (Comment : in out Comment_Data;
      List : in Comment_List;
      Builder : in Sites.Site_Builder)
   is
      use Comment_Atoms;
   begin
      Preprocess
        (Comment,
         Get_Safe_Filter
           (Builder, Comment.Atoms (Filter), List.Default_Text_Filter));
   end Preprocess;


   procedure Preprocess
     (Comment : in out Comment_Data;
      Text_Filter : in Filters.Filter'Class)
   is
      procedure Preprocess_Atom
        (Ref : in out S_Expressions.Atom_Refs.Immutable_Reference);
      procedure Preprocess_Link
        (Ref : in out S_Expressions.Atom_Refs.Immutable_Reference;
         Value : in S_Expressions.Atom);

      procedure Preprocess_Atom
        (Ref : in out S_Expressions.Atom_Refs.Immutable_Reference) is
      begin
         Reset_If_Blank (Ref);

         if not Ref.Is_Empty then
            Ref := Escapes.Escape (Ref, Escapes.HTML_Attribute);
         end if;
      end Preprocess_Atom;

      procedure Preprocess_Link
        (Ref : in out S_Expressions.Atom_Refs.Immutable_Reference;
         Value : in S_Expressions.Atom)
      is
         use type S_Expressions.Atom;
      begin
         if Is_Valid_URL (S_Expressions.To_String (Value)) then
            Ref := Escapes.Escape (Ref, Escapes.HTML_Attribute);
         elsif Is_Valid_URL ("//" & S_Expressions.To_String (Value)) then
            Ref := Escapes.Escape
              ((1 .. 2 => Character'Pos ('/')) & Value,
               Escapes.HTML_Attribute);
         else
            Ref.Reset;
         end if;
      end Preprocess_Link;

      use Comment_Atoms;
   begin
      if Comment.Flags (Comment_Flags.Preprocessed) then
         return;
      end if;

      for I in Comment_Atoms.Enum loop
         case I is
            when Filter =>
               null;

            when Link =>
               if not Comment.Atoms (I).Is_Empty then
                  Preprocess_Link (Comment.Atoms (I), Comment.Atoms (I).Query);
               end if;

            when Text =>
               Reset_If_Blank (Comment.Atoms (I));

               if not Comment.Atoms (I).Is_Empty then
                  declare
                     Buffer : S_Expressions.Atom_Buffers.Atom_Buffer;
                  begin
                     Text_Filter.Apply (Buffer, Comment.Atoms (I).Query);
                     Comment.Atoms (I) := Create (Buffer.Data);
                  end;
               end if;

            when Name | Mail | Class | Note | Title =>
               Preprocess_Atom (Comment.Atoms (I));
         end case;
      end loop;

      Comment.Flags (Comment_Flags.Preprocessed) := True;
   end Preprocess;


   function Preview_Id return S_Expressions.Atom_Refs.Immutable_Reference is
   begin
      if Preview_Id_Ref.Is_Empty then
         Preview_Id_Ref := Create (S_Expressions.To_Atom ("preview"));
      end if;

      return Preview_Id_Ref;
   end Preview_Id;


   procedure Process_Form
     (Data : in out Comment_Builder;
      Exchange : in Sites.Exchange;
      List : in Comment_List)
   is
      procedure Process (Field, Value : String);

      procedure Process (Field, Value : String) is
         use Static_Maps.Item.Form;
      begin
         Data.Raw_Fields.Insert (Field, Value);

         case Static_Maps.To_Item_Form (Field) is
            when Unknown =>
               Data.Extra_Fields.Insert (Field, Value);

               if Field /= Submit_Button and then Field /= Preview_Button then
                  Data.Has_Unknown_Field := True;
               end if;

            when Cookie_Save =>
               if Value = "yes" then
                  Data.Cookie_Save := True;
               else
                  Log (Severities.Info, "Unexpected cookie_save value """
                    & Value & '"');
               end if;

            when Date =>
               if List.Flags (List_Flags.Allow_Date_Override) then
                  begin
                     Time_IO.RFC_3339.Value
                       (Value, Data.Core.Date, Data.Core.Offset);
                  exception
                     when others => null;
                  end;
               else
                  Data.Extra_Fields.Insert (Field, Value);
                  Data.Has_Unknown_Field := True;
               end if;

            when Filter =>
               if List.Text_Filters.Is_Empty then
                  Data.Extra_Fields.Insert (Field, Value);
                  Data.Has_Unknown_Field := True;
               else
                  for Name_Ref of List.Text_Filters.Query.Data.all loop
                     if S_Expressions.To_String (Name_Ref.Query) = Value then
                        Data.Core.Atoms (Comment_Atoms.Filter) := Name_Ref;
                        exit;
                     end if;
                  end loop;
               end if;

            when Atom =>
               pragma Assert (Field'Length > 2
                 and then Field (Field'First .. Field'First + 1) = "c_");
               Data.Core.Atoms
                 (Comment_Atoms.Enum'Value
                    (Field (Field'First + 2 .. Field'Last)))
                 := Create (S_Expressions.To_Atom (Value));
         end case;
      end Process;
   begin
      Exchange.Iterate_Parameters (Process'Access);
   end Process_Form;


   overriding procedure Render
     (Exchange : in out Sites.Exchange;
      Object : in Comment_Ref;
      Expression : in out S_Expressions.Lockable.Descriptor'Class) is
   begin
      Render
        (Expression, Exchange,
         Comment_Maps.Element (Object.Container.Query.Find (Object.Id)));
   end Render;


   procedure Write
     (Comment : in Comment_Data;
      Output : in out S_Expressions.Printers.Printer'Class)
   is
      procedure Print (Key : in String; Value : in S_Expressions.Atom);
      procedure Print
        (Key : in String;
         Value : in S_Expressions.Atom_Refs.Immutable_Reference);

      procedure Print (Key : in String; Value : in S_Expressions.Atom) is
      begin
         Output.Open_List;
         Output.Append_Atom (S_Expressions.To_Atom (Key));
         Output.Append_Atom (Value);
         Output.Close_List;
      end Print;

      procedure Print
        (Key : in String;
         Value : in S_Expressions.Atom_Refs.Immutable_Reference) is
      begin
         if not Value.Is_Empty then
            Print (Key, Value.Query);
         end if;
      end Print;

      use type Comment_Atoms.Enum;
   begin
      pragma Assert (not Comment.Flags (Comment_Flags.Preprocessed));

      Print ("date", S_Expressions.To_Atom
        (Time_IO.RFC_3339.Image (Comment.Date, Comment.Offset)));

      for I in Comment_Atoms.Enum loop
         Print (Image (I), Comment.Atoms (I));
      end loop;

      Print_Flags :
      declare
         Flag_Printed : Boolean := False;
      begin
         for Flag in Comment_Flags.Enum loop
            if Comment.Flags (Flag) then
               if not Flag_Printed then
                  Output.Open_List;
                  Output.Append_String ("flags");
                  Flag_Printed := True;
               end if;

               Output.Append_Atom (Comment_Flag_IO.Image (Flag));
            end if;
         end loop;

         if Flag_Printed then
            Output.Close_List;
         end if;
      end Print_Flags;
   end Write;


   procedure Write
     (Builder : in Comment_Builder;
      Output : in out S_Expressions.Printers.Printer'Class) is
   begin
      Output.Open_List;
      Output.Append_Atom (Builder.Core.Id.Query);

      Write (Builder.Core, Output);

      Write_Extra_Fields :
      declare
         Cursor : String_Maps.Cursor := Builder.Extra_Fields.First;
      begin
         Output.Open_List;
         Output.Append_String ("extra-fields");

         while String_Maps.Has_Element (Cursor) loop
            Output.Open_List;
            Output.Append_String (String_Maps.Key (Cursor));
            Output.Append_String (String_Maps.Element (Cursor));
            Output.Close_List;
            String_Maps.Next (Cursor);
         end loop;

         Output.Close_List;
      end Write_Extra_Fields;

      Output.Open_List;
      Output.Append_String ("has-unknown-field");
      Output.Append_String (Boolean'Image (Builder.Has_Unknown_Field));
      Output.Close_List;

      Output.Open_List;
      Output.Append_String ("action");
      Output.Append_Atom (Post_Action_IO.Image (Builder.Action));
      if not Builder.Reason.Is_Empty then
         Output.Append_Atom (Builder.Reason.Query);
      end if;
      Output.Close_List;

      if not Builder.Anchor.Is_Empty then
         Output.Open_List;
         Output.Append_String ("anchor");
         Output.Append_Atom (Builder.Anchor.Query);
         Output.Close_List;
      end if;

      Output.Close_List;
   end Write;


   procedure Write
     (Builder : in Comment_Builder;
      Site : in Sites.Site;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class)
   is
      use type S_Expressions.Events.Event;
   begin
      if Arguments.Current_Event /= S_Expressions.Events.Add_Atom then
         Log (Severities.Error,
           "Invalid write target: first name is not an atom");
         return;
      end if;

      declare
         Stream : aliased File_Streams.File_Stream := File_Streams.Open
           (Ada.Streams.Stream_IO.Append_File,
            S_Expressions.To_String (Arguments.Current_Atom));
         Printer : S_Expressions.Printers.Pretty.Stream_Printer
           (Stream'Access);
      begin
         Site.Set_Parameters (Printer);
         Write (Builder, Printer);
         Printer.Newline;
      end;
   end Write;



   ------------------------------
   -- Comment List Subprograms --
   ------------------------------

   overriding procedure Finalize (Object : in out Comment_List) is
   begin
      if not Object.Comments.Is_Empty then
         Object.Comments.Update.Orphan;
         Object.Comments.Reset;
      end if;
   end Finalize;


   procedure Load
     (Object : in out Comment_List;
      Builder : in out Sites.Site_Builder;
      Parent : in Tags.Visible_Access := null;
      Parent_Path : in S_Expressions.Atom_Refs.Immutable_Reference
        := S_Expressions.Atom_Refs.Null_Immutable_Reference) is
   begin
      if Object.Backend_Name.Is_Empty or else Object.Backend_Path.Is_Empty then
         return;
      end if;

      Object.Parent_Path := Parent_Path;

      declare
         procedure Process (Name : in S_Expressions.Atom);

         Backend : constant Backends.Backend'Class
           := Sites.Get_Backend (Builder, Object.Backend_Name.Query);
         Directory : constant S_Expressions.Atom_Refs.Accessor
           := Object.Backend_Path.Query;
         Map : Comment_Maps.Unsafe_Maps.Map;

         procedure Process (Name : in S_Expressions.Atom) is
            Input_Stream : aliased Ada.Streams.Root_Stream_Type'Class
              := Backend.Read (Directory, Name);
            Reader : S_Expressions.Parsers.Stream_Parser (Input_Stream'Access);
            Comment : Comment_Data;
            Position : Comment_Maps.Unsafe_Maps.Cursor;
            Inserted : Boolean;
         begin
            Reader.Next;
            Update (Reader, Comment, Meaningless_Value);

            if not Comment.Flags (Comment_Flags.Ignored) then
               Comment.Id := Create (Name);
               Comment.Parent := Parent;
               Preprocess (Comment, Object, Builder);

               Map.Insert (Name, Comment, Position, Inserted);

               if not Inserted then
                  Log (Severities.Error, "Duplicate comment id """
                    & S_Expressions.To_String (Name) & '"');
               end if;
            end if;
         end Process;
      begin
         Backend.Iterate (Directory, Process'Access);
         Object.Comments := Container_Refs.Create (new Comment_Container);
         Object.Comments.Update.Initialize (Map, Parent);
         Object.Parent := Parent;
      end;

      if not Object.Tags.Is_Empty then
         declare
            C : Comment_Maps.Cursor := Object.Comments.Query.First;
            Id : S_Expressions.Atom_Refs.Immutable_Reference;
         begin
            while Comment_Maps.Has_Element (C) loop
               Id := Comment_Maps.Element (C).Id;
               Sites.Insert
                 (Builder,
                  Tags.Create (Object.Tags.Query, Id),
                  Comment_Ref'(Object.Comments, Id));
               Comment_Maps.Next (C);
            end loop;
         end;
      end if;
   end Load;


   procedure Render
     (Exchange : in out Sites.Exchange;
      Object : in Comment_List;
      Expression : in out S_Expressions.Lockable.Descriptor'Class) is
   begin
      if not Object.Backend_Name.Is_Empty
        and then not Object.Backend_Path.Is_Empty
      then
         Render (Expression, Exchange, Object);
      end if;
   end Render;


   procedure Respond
     (List : in out Comment_List;
      Exchange : in out Sites.Exchange;
      Extra_Path : in S_Expressions.Atom)
   is
      function Get_Anchor
        (Builder : Comment_Builder;
         Default : S_Expressions.Atom)
        return S_Expressions.Atom;
      function Redirect_Base
        (Builder : Comment_Builder)
        return S_Expressions.Atom;
      function Redirect_Location
        (Default_Anchor : S_Expressions.Atom := S_Expressions.Null_Atom)
        return S_Expressions.Atom;

      function Get_Anchor
        (Builder : Comment_Builder;
         Default : S_Expressions.Atom)
        return S_Expressions.Atom is
      begin
         if Builder.Anchor.Is_Empty then
            return Default;
         else
            return Builder.Anchor.Query;
         end if;
      end Get_Anchor;

      function Redirect_Base
        (Builder : Comment_Builder)
        return S_Expressions.Atom is
      begin
         if Builder.Redirect_Base.Is_Empty then
            return List.Parent_Path.Query;
         else
            return Builder.Redirect_Base.Query;
         end if;
      end Redirect_Base;

      Builder : Comment_Builder;

      function Redirect_Location
        (Default_Anchor : S_Expressions.Atom := S_Expressions.Null_Atom)
        return S_Expressions.Atom
      is
         use type Ada.Streams.Stream_Element_Array;

         Anchor : constant S_Expressions.Atom
           := Get_Anchor (Builder, Default_Anchor);
      begin
         if Anchor'Length > 0 then
            return Redirect_Base (Builder) & Character'Pos ('#') & Anchor;
         else
            return Redirect_Base (Builder);
         end if;
      end Redirect_Location;
   begin
      if Extra_Path'Length > 0
        or else List.Flags (List_Flags.Closed)
        or else List.Backend_Name.Is_Empty
        or else List.Backend_Path.Is_Empty
        or else List.Parent_Path.Is_Empty
      then
         return;
      end if;

      Check_Method :
      declare
         Allowed : Boolean;
      begin
         Error_Pages.Check_Method (Exchange, Exchanges.POST, Allowed);

         if not Allowed then
            return;
         end if;
      end Check_Method;

      if Exchange.Parameter (Preview_Button) /= "" then
         if not Tags."=" (List.Parent, null) then
            Render_Default (Exchange, List.Parent.all);
         end if;
         return;
      elsif Exchange.Parameter (Ignore_Button) /= ""
        and then List.Flags (List_Flags.Allow_Ignore)
      then
         declare
            Req_Id : constant S_Expressions.Atom
              := S_Expressions.To_Atom (Exchange.Parameter ("id"));
            Ref : S_Expressions.Atom_Refs.Immutable_Reference;
         begin
            List.Comments.Update.Ignore (Req_Id, Ref);
            if not Ref.Is_Empty then
               Update_Stored_Comment :
               declare
                  Backend : Backends.Backend'Class
                    := Exchange.Site.Get_Backend (List.Backend_Name.Query);
                  Stream : aliased Ada.Streams.Root_Stream_Type'Class
                    := Backend.Append
                       (List.Backend_Path.Query,
                        Ref.Query);
                  Printer : S_Expressions.Printers.Pretty.Stream_Printer
                    (Stream'Access);
               begin
                  Exchange.Site.Set_Parameters (Printer);
                  Printer.Open_List;
                  Printer.Append_String ("flags");
                  Printer.Append_Atom
                    (Comment_Flag_IO.Image (Comment_Flags.Ignored));
                  Printer.Close_List;
               end Update_Stored_Comment;

               if not List.Tags.Is_Empty then
                  Exchange.Site.Queue_Update (Comment_Remover'
                    (Container => List.Comments,
                     Id => Ref,
                     Tags => List.Tags));
               end if;
            end if;
         end;
         Error_Pages.See_Other (Exchange, Redirect_Location);
         return;
      elsif Exchange.Parameter (Submit_Button) = "" then
         return;
      end if;

      Builder.Action := Save_Comment;
      Builder.Core.Date := Ada.Calendar.Clock;
      Builder.Core.Offset := Ada.Calendar.Time_Zones.UTC_Time_Offset
        (Builder.Core.Date);
      Builder.Core.Id := Create (S_Expressions.To_Atom
        (Time_Keys.To_Key (Builder.Core.Date)));
      Builder.Core.Flags (Comment_Flags.Ignored)
        := List.Flags (List_Flags.Ignore_By_Default);
      Process_Form (Builder, Exchange, List);
      Process_Actions (Builder, Exchange.Site.all, List.Post_Filter);

      if not Builder.Reason.Is_Empty then
         Log (Severities.Info, "Comment tagged "
           & Post_Action'Image (Builder.Action)
           & " because "
           & S_Expressions.To_String (Builder.Reason.Query));
      end if;

      case Builder.Action is
         when Force_Preview =>
            if not Tags."=" (List.Parent, null) then
               Render_Default (Exchange, List.Parent.all);
            end if;

         when Parent_Redirect =>
            Error_Pages.See_Other (Exchange, Redirect_Location);

         when Save_Comment =>
            Write_Comment :
            declare
               Backend : Backends.Backend'Class
                 := Exchange.Site.Get_Backend (List.Backend_Name.Query);
               Stream : aliased Ada.Streams.Root_Stream_Type'Class
                 := Backend.Create
                    (List.Backend_Path.Query,
                     Builder.Core.Id.Query);
               Printer : S_Expressions.Printers.Pretty.Stream_Printer
                 (Stream'Access);
            begin
               Exchange.Site.Set_Parameters (Printer);
               Write (Builder.Core, Printer);
            end Write_Comment;

            if Builder.Cookie_Save then
               Set_Cookie :
               declare
                  Info : constant Comment_Cookies.Comment_Info
                    := Comment_Cookies.Create
                       (Name => Builder.Core.Atoms (Comment_Atoms.Name),
                        Mail => Builder.Core.Atoms (Comment_Atoms.Mail),
                        Link => Builder.Core.Atoms (Comment_Atoms.Link),
                        Filter => Builder.Core.Atoms (Comment_Atoms.Filter));
               begin
                  Sites.Set_Comment_Cookie (Exchange, Info);
               end Set_Cookie;
            end if;

            Error_Pages.See_Other
              (Exchange,
               Redirect_Location (Builder.Core.Id.Query));

            if not Builder.Core.Flags (Comment_Flags.Ignored) then
               Preprocess (Builder.Core, List, Exchange.Site.all);
               List.Comments.Update.Insert (Builder.Core);

               if not List.Tags.Is_Empty then
                  Exchange.Site.Queue_Update (Comment_Inserter'
                    (Container => List.Comments,
                     New_Item => Builder.Core,
                     Tags => List.Tags));
               end if;
            end if;
      end case;
   end Respond;


   procedure Set
     (List : out Comment_List;
      Expression : in out S_Expressions.Lockable.Descriptor'Class) is
   begin
      List := Empty_List;
      Update (Expression, List, Meaningless_Value);
   end Set;


   procedure Set_Parent
     (Container : in out Comment_Maps.Updatable_Map;
      Parent : in Tags.Visible_Access) is
   begin
      for E of Container loop
         E.Parent := Parent;
      end loop;
   end Set_Parent;


   procedure Update_Ranks (Container : in out Comment_Maps.Updatable_Map) is
      Current_Rank : Positive := 1;
   begin
      for E of Container loop
         E.Rank := Current_Rank;
         Current_Rank := Current_Rank + 1;
      end loop;

      pragma Assert (Current_Rank = Natural (Container.Length) + 1);
   end Update_Ranks;



   -----------------------
   -- Comment Container --
   -----------------------

   protected body Comment_Container is

      procedure Initialize
        (Data : in Comment_Maps.Unsafe_Maps.Map;
         Parent : in Tags.Visible_Access) is
      begin
         Map := Comment_Maps.Create (Data);
         Set_Parent (Map, Parent);
         Update_Ranks (Map);
         Comment_Container.Parent := Parent;
      end Initialize;


      procedure Insert (Data : in Comment_Data) is
         New_Item : Comment_Data := Data;
      begin
         New_Item.Parent := Parent;
         Map := Comment_Maps.Insert (Map, New_Item.Id.Query, New_Item);
         Update_Ranks (Map);
      end Insert;


      procedure Ignore
        (Id : in S_Expressions.Atom;
         Ref : out S_Expressions.Atom_Refs.Immutable_Reference)
      is
         Position : constant Comment_Maps.Cursor := Map.Find (Id);
      begin
         if Comment_Maps.Has_Element (Position) then
            Ref := Comment_Maps.Element (Position).Id;
            Map := Comment_Maps.Delete (Map, Position);
            Update_Ranks (Map);
         else
            Ref := S_Expressions.Atom_Refs.Null_Immutable_Reference;
            Log (Severities.Error,
              "Unknown comment id """
               & S_Expressions.To_String (Id)
               & """ to ignore");
         end if;
      end Ignore;


      procedure Orphan is
      begin
         Set_Parent (Map, null);
         Parent := null;
      end Orphan;


      function Find (Id : S_Expressions.Atom_Refs.Immutable_Reference)
        return Comment_Maps.Cursor is
      begin
         return Map.Find (Id.Query);
      end Find;


      function First return Comment_Maps.Cursor is
      begin
         return Map.First;
      end First;


      function Iterate return
        Comment_Maps.Map_Iterator_Interfaces.Reversible_Iterator'Class is
      begin
         return Map.Iterate;
      end Iterate;


      function Length return Natural is
      begin
         return Natural (Map.Length);
      end Length;

   end Comment_Container;



   -------------------
   -- Site Updaters --
   -------------------

   overriding procedure Update
     (Self : in Comment_Inserter;
      Site : in out Sites.Site) is
   begin
      pragma Assert (not Self.Tags.Is_Empty);

      Site.Insert
        (Tags.Create (Self.Tags.Query, Self.New_Item.Id),
         Comment_Ref'(Self.Container, Self.New_Item.Id));
   end Update;


   overriding procedure Update
     (Self : in Comment_Remover;
      Site : in out Sites.Site) is
   begin
      pragma Assert (not Self.Tags.Is_Empty);

      Site.Remove
        (Tags.Create (Self.Tags.Query, Self.Id),
         Comment_Ref'(Self.Container, Self.Id));
   end Update;

end Natools.Web.Comments;
