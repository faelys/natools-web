------------------------------------------------------------------------------
-- Copyright (c) 2015, Natacha Porté                                        --
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
with Natools.Web.Error_Pages;
with Natools.Web.Escapes;
with Natools.Web.Exchanges;
with Natools.Web.Fallback_Render;
with Natools.Web.Filters.Stores;
with Natools.Web.Filters.Text_Blocks;
with Natools.Web.Is_Valid_URL;
with Natools.Web.List_Templates;
with Natools.Web.Render_Default;
with Natools.Web.Sites.Updates;

package body Natools.Web.Comments is

   package Static_Maps renames Natools.Static_Maps.Web.Comments;

   package Comment_Flag_IO is new S_Expressions.Enumeration_IO.Typed_IO
     (Comment_Flags.Enum);

   package Comment_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (S_Expressions.Atom, Comment_Data, S_Expressions."<");

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
      Action : Post_Action;
      Reason : S_Expressions.Atom_Refs.Immutable_Reference;
      Anchor : S_Expressions.Atom_Refs.Immutable_Reference;
   end record;


   Invalid_Condition : exception;


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
      Context : in Comment_Ref;
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

   procedure Render_Comment_Position
     (Exchange : in out Sites.Exchange;
      Context : in Comment_Ref;
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
     (Sites.Exchange, Comment_Ref, Render_Comment_Position, Append);

   procedure Render_List is new List_Templates.Render (Comment_Iterators);

   procedure Update is new S_Expressions.Interpreter_Loop
     (Comment_Data, Meaningless_Type, Update_Item);

   procedure Update is new S_Expressions.Interpreter_Loop
     (Comment_List, Meaningless_Type, Update_List);



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
      Context : in Comment_Ref;
      Data : in S_Expressions.Atom)
   is
      pragma Unreferenced (Context);
   begin
      Exchange.Append (Data);
   end Append;


   procedure Render_Comment_Position
     (Exchange : in out Sites.Exchange;
      Context : in Comment_Ref;
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

      procedure Re_Enter
        (Exchange : in out Sites.Exchange;
         Expression : in out S_Expressions.Lockable.Descriptor'Class) is
      begin
         Render (Expression, Exchange, Context);
      end Re_Enter;

      procedure Render_Ref
        (Ref : in S_Expressions.Atom_Refs.Immutable_Reference) is
      begin
         if not Ref.Is_Empty then
            Exchange.Append (Ref.Query);
         elsif Arguments.Current_Event
           in S_Expressions.Events.Add_Atom | S_Expressions.Events.Open_List
         then
            Render (Arguments, Exchange, Context);
         end if;
      end Render_Ref;

      Accessor : constant Comment_Array_Refs.Accessor := Context.List.Query;
      Comment : Comment_Data renames Accessor.Data.Data (Context.Position);
   begin
      pragma Assert (Comment.Flags (Comment_Flags.Preprocessed));

      case Static_Maps.To_Item_Command (S_Expressions.To_String (Name)) is
         when Unknown =>
            Fallback_Render
              (Exchange, Name, Arguments, "comment", Re_Enter'Access);

         when Date =>
            S_Expressions.Templates.Dates.Render
              (Exchange, Arguments, Comment.Date, Comment.Offset);

         when Id =>
            Exchange.Append (Comment.Id.Query);

         when If_Link =>
            if not Comment.Link.Is_Empty then
               Render (Arguments, Exchange, Context);
            end if;

         when If_No_Link =>
            if Comment.Link.Is_Empty then
               Render (Arguments, Exchange, Context);
            end if;

         when If_No_Mail =>
            if Comment.Mail.Is_Empty then
               Render (Arguments, Exchange, Context);
            end if;

         when If_No_Name =>
            if Comment.Name.Is_Empty then
               Render (Arguments, Exchange, Context);
            end if;

         when If_Mail =>
            if not Comment.Mail.Is_Empty then
               Render (Arguments, Exchange, Context);
            end if;

         when If_Name =>
            if not Comment.Name.Is_Empty then
               Render (Arguments, Exchange, Context);
            end if;

         when Static_Maps.Item.Command.Name =>
            Render_Ref (Comment.Name);

         when Mail =>
            Render_Ref (Comment.Mail);

         when Link =>
            Render_Ref (Comment.Link);

         when Parent =>
            if Accessor.Parent /= null then
               Tags.Render (Exchange, Accessor.Parent.all, Arguments);
            end if;

         when Rank =>
            S_Expressions.Templates.Integers.Render
              (Exchange,
               Arguments,
               Integer (Context.Position));

         when Text =>
            Render_Ref (Comment.Text);
      end case;
   end Render_Comment_Position;


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
               Comment_Range'(List => List.Comments),
               List_Templates.Read_Parameters (Arguments));

         when Parent =>
            declare
               use type Tags.Visible_Access;

               Accessor : constant Comment_Array_Refs.Accessor
                 := List.Comments.Query;
            begin
               if Accessor.Parent /= null then
                  Tags.Render (Exchange, Accessor.Parent.all, Arguments);
               end if;
            end;

         when Preview =>
            if Exchange.Parameter (Preview_Button) = ""
              and then Exchange.Parameter (Submit_Button) = ""
            then
               return;
            end if;

            declare
               Ref : constant Comment_Ref
                 := (List => Comment_Array_Refs.Create (new Comment_Container'
                       (Size => 1,
                        Data => (1 => <>),
                        Parent => List.Comments.Query.Parent)),
                     Position => 1);
               Builder : Comment_Builder;
            begin
               Builder.Core.Date := Ada.Calendar.Clock;
               Builder.Core.Offset := Ada.Calendar.Time_Zones.UTC_Time_Offset
                 (Builder.Core.Date);
               Builder.Core.Id := Preview_Id;
               Process_Form (Builder, Exchange, List);
               Preprocess (Builder.Core, List, Exchange.Site.all);
               Ref.List.Update.Data (1) := Builder.Core;
               Render (Arguments, Exchange, Ref);
            end;

         when Size =>
            S_Expressions.Templates.Integers.Render
              (Exchange,
               Arguments,
               Integer (List.Comments.Query.Data.Data'Length));
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
            Log (Severities.Error,
              "Unknown comment element """
               & S_Expressions.To_String (Name) & '"');

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

         when Static_Maps.Item.Element.Name =>
            Comment.Name := Create (Arguments.Current_Atom);

         when Mail =>
            Comment.Mail := Create (Arguments.Current_Atom);

         when Link =>
            Comment.Link := Create (Arguments.Current_Atom);

         when Text =>
            Comment.Text := Create (Arguments.Current_Atom);

         when Text_Filter =>
            Comment.Text_Filter := Create (Arguments.Current_Atom);
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

         when Link =>
            return String_Evaluate (Builder.Core.Link, Arguments);

         when Mail =>
            return String_Evaluate (Builder.Core.Mail, Arguments);

         when Static_Maps.Item.Condition.Name =>
            return String_Evaluate (Builder.Core.Name, Arguments);

         when Text =>
            return String_Evaluate (Builder.Core.Text, Arguments);
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
            raise Invalid_Condition with "Unknown simple conditional """
              & S_Expressions.To_String (Name) & '"';

         when Has_Extra_Fields =>
            return not Builder.Extra_Fields.Is_Empty;

         when Has_Unknown_Field =>
            return Builder.Has_Unknown_Field;

         when Link =>
            return not Builder.Core.Link.Is_Empty;

         when Mail =>
            return not Builder.Core.Mail.Is_Empty;

         when Static_Maps.Item.Condition.Name =>
            return not Builder.Core.Name.Is_Empty;

         when Text =>
            return not Builder.Core.Text.Is_Empty;
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
      Site : in Sites.Site) is
   begin
      Preprocess
        (Comment,
         Get_Safe_Filter
           (Site, Comment.Text_Filter, List.Default_Text_Filter));
   end Preprocess;


   procedure Preprocess
     (Comment : in out Comment_Data;
      List : in Comment_List;
      Builder : in Sites.Site_Builder) is
   begin
      Preprocess
        (Comment,
         Get_Safe_Filter
           (Builder, Comment.Text_Filter, List.Default_Text_Filter));
   end Preprocess;


   procedure Preprocess
     (Comment : in out Comment_Data;
      Text_Filter : in Filters.Filter'Class) is
   begin
      if Comment.Flags (Comment_Flags.Preprocessed) then
         return;
      end if;

      Reset_If_Blank (Comment.Name);

      if not Comment.Name.Is_Empty then
         Comment.Name := Escapes.Escape (Comment.Name, Escapes.HTML_Attribute);
      end if;

      Reset_If_Blank (Comment.Mail);

      if not Comment.Mail.Is_Empty then
         Comment.Mail := Escapes.Escape (Comment.Mail, Escapes.HTML_Attribute);
      end if;

      if not Comment.Link.Is_Empty
        and then Is_Valid_URL (S_Expressions.To_String (Comment.Link.Query))
      then
         Comment.Link := Escapes.Escape (Comment.Link, Escapes.HTML_Attribute);
      else
         Comment.Link.Reset;
      end if;

      Reset_If_Blank (Comment.Text);

      if not Comment.Text.Is_Empty then
         declare
            Buffer : S_Expressions.Atom_Buffers.Atom_Buffer;
         begin
            Text_Filter.Apply (Buffer, Comment.Text.Query);
            Comment.Text := Create (Buffer.Data);
         end;
      end if;

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
                        Data.Core.Text_Filter := Name_Ref;
                        exit;
                     end if;
                  end loop;
               end if;

            when Name =>
               Data.Core.Name := Create (S_Expressions.To_Atom (Value));

            when Mail =>
               Data.Core.Mail := Create (S_Expressions.To_Atom (Value));

            when Link =>
               Data.Core.Link := Create (S_Expressions.To_Atom (Value));

            when Text =>
               Data.Core.Text := Create (S_Expressions.To_Atom (Value));
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
      Render (Expression, Exchange, Object);
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
   begin
      pragma Assert (not Comment.Flags (Comment_Flags.Preprocessed));

      Print ("date", S_Expressions.To_Atom
        (Time_IO.RFC_3339.Image (Comment.Date, Comment.Offset)));

      Print ("name", Comment.Name);
      Print ("mail", Comment.Mail);
      Print ("link", Comment.Link);
      Print ("text", Comment.Text);
      Print ("text-filter", Comment.Text_Filter);

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
         Object.Comments.Update.Parent := null;
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
         function Create return Comment_Container;
         procedure Process (Name : in S_Expressions.Atom);

         Backend : constant Backends.Backend'Class
           := Sites.Get_Backend (Builder, Object.Backend_Name.Query);
         Directory : constant S_Expressions.Atom_Refs.Accessor
           := Object.Backend_Path.Query;
         Map : Comment_Maps.Map;

         function Create return Comment_Container is
            Cursor : Comment_Maps.Cursor := Map.First;
         begin
            return Result : Comment_Container
              (S_Expressions.Offset (Map.Length))
            do
               Result.Parent := Parent;
               for I in Result.Data'Range loop
                  Result.Data (I) := Comment_Maps.Element (Cursor);
                  Comment_Maps.Next (Cursor);
               end loop;
               pragma Assert (not Comment_Maps.Has_Element (Cursor));
            end return;
         end Create;

         procedure Process (Name : in S_Expressions.Atom) is
            Input_Stream : aliased Ada.Streams.Root_Stream_Type'Class
              := Backend.Read (Directory, Name);
            Reader : S_Expressions.Parsers.Stream_Parser (Input_Stream'Access);
            Comment : Comment_Data;
            Position : Comment_Maps.Cursor;
            Inserted : Boolean;
         begin
            Reader.Next;
            Update (Reader, Comment, Meaningless_Value);

            if not Comment.Flags (Comment_Flags.Ignored) then
               Comment.Id := Create (Name);
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
         Object.Comments := Comment_Array_Refs.Create (Create'Access);
      end;

      if not Object.Tags.Is_Empty then
         declare
            Accessor : constant Comment_Array_Refs.Accessor
              := Object.Comments.Query;
         begin
            for I in Accessor.Data.Data'Range loop
               Sites.Insert
                 (Builder,
                  Tags.Create (Object.Tags.Query, Accessor.Data (I).Id),
                  Comment_Ref'(Object.Comments, I));
            end loop;
         end;
      end if;
   end Load;


   procedure Render
     (Exchange : in out Sites.Exchange;
      Object : in Comment_List;
      Expression : in out S_Expressions.Lockable.Descriptor'Class) is
   begin
      if not Object.Comments.Is_Empty then
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

      Builder : Comment_Builder;

      function Redirect_Location
        (Default_Anchor : S_Expressions.Atom := S_Expressions.Null_Atom)
        return S_Expressions.Atom
      is
         use type Ada.Streams.Stream_Element_Array;

         Parent : constant S_Expressions.Atom := List.Parent_Path.Query;
         Anchor : constant S_Expressions.Atom
           := Get_Anchor (Builder, Default_Anchor);
      begin
         if Anchor'Length > 0 then
            return Parent & Character'Pos ('#') & Anchor;
         else
            return Parent;
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
         if not Tags."=" (List.Comments.Query.Parent, null) then
            Render_Default (Exchange, List.Comments.Query.Parent.all);
         end if;
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
            if not Tags."=" (List.Comments.Query.Parent, null) then
               Render_Default (Exchange, List.Comments.Query.Parent.all);
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

            Error_Pages.See_Other
              (Exchange,
               Redirect_Location (Builder.Core.Id.Query));

            if not Builder.Core.Flags (Comment_Flags.Ignored) then
               Sites.Updates.Reload (Exchange.Site.all);
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



   --------------------------
   -- Iterator Subprograms --
   --------------------------

   procedure Render
     (Exchange : in out Sites.Exchange;
      Position : in Cursor;
      Expression : in out S_Expressions.Lockable.Descriptor'Class) is
   begin
      Render (Expression, Exchange, Position.Value);
   end Render;


   overriding function First (Object : Comment_Range) return Cursor is
   begin
      return (Value => (Object.List, Object.List.Query.Data.Data'First));
   end First;


   overriding function Last (Object : Comment_Range) return Cursor is
   begin
      return (Value => (Object.List, Object.List.Query.Data.Data'Last));
   end Last;


   overriding function Next (Object : Comment_Range; Position : Cursor)
     return Cursor
   is
      pragma Unreferenced (Object);
      use type S_Expressions.Offset;
   begin
      return (Value => (Position.Value.List, Position.Value.Position + 1));
   end Next;


   overriding function Previous (Object : Comment_Range; Position : Cursor)
     return Cursor
   is
      pragma Unreferenced (Object);
      use type S_Expressions.Offset;
   begin
      return (Value => (Position.Value.List, Position.Value.Position - 1));
   end Previous;

end Natools.Web.Comments;
