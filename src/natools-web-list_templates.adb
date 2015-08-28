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
with Natools.S_Expressions.Interpreter_Loop;
with Natools.Static_Maps.Web.List_Templates;

package body Natools.Web.List_Templates is

   procedure Execute_Atom
     (State : in out Parameters;
      Context : in Meaningless_Type;
      Name : in S_Expressions.Atom);

   procedure Execute_Expression
     (State : in out Parameters;
      Context : in Meaningless_Type;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class);

   procedure Set_Or_Reset
     (Ref : in out S_Expressions.Atom_Refs.Immutable_Reference;
      Expression : in out S_Expressions.Lockable.Descriptor'Class);
      --  if Expression has an atom, copy it into Ref, otherwise reset Ref


   ---------------------------
   -- Parameter Interpreter --
   ---------------------------

   procedure Execute_Expression
     (State : in out Parameters;
      Context : in Meaningless_Type;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class)
   is
      pragma Unreferenced (Context);

      package Commands renames Natools.Static_Maps.Web.List_Templates;

      use type S_Expressions.Events.Event;
   begin
      case Commands.To_Command (S_Expressions.To_String (Name)) is
         when Commands.Unknown_Command =>
            null;

         when Commands.Backward =>
            State.Going := Backward;

         when Commands.Ellipses_Are_Items =>
            State.Ellipses_Are_Items := True;

         when Commands.Ellipses_Are_Not_Items =>
            State.Ellipses_Are_Items := False;

         when Commands.Ellipsis_Prefix =>
            Set_Or_Reset (State.Ellipsis_Prefix, Arguments);

         when Commands.Ellipsis_Suffix =>
            Set_Or_Reset (State.Ellipsis_Suffix, Arguments);

         when Commands.Forward =>
            State.Going := Forward;

         when Commands.If_Empty =>
            Set_Or_Reset (State.If_Empty, Arguments);

         when Commands.Length_Limit =>
            if Arguments.Current_Event = S_Expressions.Events.Add_Atom then
               begin
                  State.Limit := Count'Value (S_Expressions.To_String
                                               (Arguments.Current_Atom));
               exception
                  when Constraint_Error =>
                     State.Limit := 0;
               end;
            end if;

         when Commands.Prefix =>
            Set_Or_Reset (State.Prefix, Arguments);

         when Commands.Show_Beginning =>
            State.Shown_End := Beginning;

         when Commands.Show_Ending =>
            State.Shown_End := Ending;

         when Commands.Suffix =>
            Set_Or_Reset (State.Suffix, Arguments);

         when Commands.Separator =>
            Set_Or_Reset (State.Separator, Arguments);

         when Commands.Template =>
            State.Template := S_Expressions.Caches.Move (Arguments);
      end case;
   end Execute_Expression;


   procedure Execute_Atom
     (State : in out Parameters;
      Context : in Meaningless_Type;
      Name : in S_Expressions.Atom)
   is
      pragma Unreferenced (Context);

      package Commands renames Natools.Static_Maps.Web.List_Templates;
   begin
      case Commands.To_Command (S_Expressions.To_String (Name)) is
         when Commands.Unknown_Command =>
            null;

         when Commands.Backward =>
            State.Going := Backward;

         when Commands.Ellipses_Are_Items =>
            State.Ellipses_Are_Items := True;

         when Commands.Ellipses_Are_Not_Items =>
            State.Ellipses_Are_Items := False;

         when Commands.Ellipsis_Prefix =>
            State.Ellipsis_Prefix.Reset;

         when Commands.Ellipsis_Suffix =>
            State.Ellipsis_Suffix.Reset;

         when Commands.Forward =>
            State.Going := Forward;

         when Commands.If_Empty =>
            State.If_Empty.Reset;

         when Commands.Length_Limit =>
            State.Limit := 0;

         when Commands.Prefix =>
            State.Prefix.Reset;

         when Commands.Show_Beginning =>
            State.Shown_End := Beginning;

         when Commands.Show_Ending =>
            State.Shown_End := Ending;

         when Commands.Separator =>
            State.Separator.Reset;

         when Commands.Suffix =>
            State.Suffix.Reset;

         when Commands.Template =>
            null;
      end case;
   end Execute_Atom;


   procedure Read is new Natools.S_Expressions.Interpreter_Loop
     (Parameters, Meaningless_Type, Execute_Expression, Execute_Atom);


   procedure Read_Parameters
     (Object : in out Parameters;
      Expression : in out S_Expressions.Lockable.Descriptor'Class) is
   begin
      Read (Expression, Object, Meaningless_Value);
   end Read_Parameters;


   function Read_Parameters
     (Expression : in out S_Expressions.Lockable.Descriptor'Class)
     return Parameters
   is
      Result : Parameters;
   begin
      Read_Parameters (Result, Expression);
      return Result;
   end Read_Parameters;


   procedure Set_Or_Reset
     (Ref : in out S_Expressions.Atom_Refs.Immutable_Reference;
      Expression : in out S_Expressions.Lockable.Descriptor'Class)
   is
      package Constructors renames Natools.S_Expressions.Atom_Ref_Constructors;
   begin
      case Expression.Current_Event is
         when S_Expressions.Events.Add_Atom =>
            Ref := Constructors.Create (Expression.Current_Atom);

         when S_Expressions.Events.Open_List
           | S_Expressions.Events.Close_List
           | S_Expressions.Events.End_Of_Input
           | S_Expressions.Events.Error
         =>
            Ref.Reset;
      end case;
   end Set_Or_Reset;



   -----------------------
   -- Generic Rendering --
   -----------------------

   procedure Render
     (Exchange : in out Sites.Exchange;
      Iterator : in Iterators.Reversible_Iterator'Class;
      Param : in Parameters)
   is
      procedure Ending_Showing_Loop
        (Position : in Cursor;
         Remaining_Depth : in Count);
      procedure Loop_Body
        (Position : in Cursor; Exit_Loop : out Boolean);

      Rendered : Count := 0;
      Extra_Items : constant Count
        := (if Param.Ellipses_Are_Items
            then (if Param.Ellipsis_Prefix.Is_Empty then 0 else 1)
               + (if Param.Ellipsis_Suffix.Is_Empty then 0 else 1)
            else 0);
      Is_First : Boolean := True;

      procedure Ending_Showing_Loop
        (Position : in Cursor;
         Remaining_Depth : in Count) is
      begin
         if Remaining_Depth > 1 then
            case Param.Going is
               when Backward =>
                  Ending_Showing_Loop
                    (Iterator.Next (Position), Remaining_Depth - 1);
               when Forward =>
                  Ending_Showing_Loop
                    (Iterator.Previous (Position), Remaining_Depth - 1);
            end case;

            if not Param.Separator.Is_Empty then
               Exchange.Append (Param.Separator.Query);
            end if;
         end if;

         declare
            Template_Copy : S_Expressions.Caches.Cursor := Param.Template;
         begin
            Render (Exchange, Position, Template_Copy);
         end;
      end Ending_Showing_Loop;

      procedure Loop_Body
        (Position : in Cursor; Exit_Loop : out Boolean) is
      begin
         if Param.Limit > 0 and then Rendered >= Param.Limit then
            if not Param.Ellipsis_Suffix.Is_Empty then
               Exchange.Append (Param.Ellipsis_Suffix.Query);
            end if;

            Exit_Loop := True;
            return;
         end if;

         Exit_Loop := False;

         if not Is_First and then not Param.Separator.Is_Empty then
            Exchange.Append (Param.Separator.Query);
         end if;

         declare
            Template_Copy : S_Expressions.Caches.Cursor := Param.Template;
         begin
            Render (Exchange, Position, Template_Copy);
         end;

         Rendered := Rendered + 1;
         Is_First := False;
      end Loop_Body;

      Exit_Loop : Boolean;
      Seen : Count := 0;
   begin
      if Param.Shown_End = Ending and then Param.Limit > 0 then
         if Seen = 0 then
            for I in Iterator loop
               Seen := Seen + 1;
               exit when Seen > Param.Limit;
            end loop;

            if Seen = 0 then
               if not Param.If_Empty.Is_Empty then
                  Exchange.Append (Param.If_Empty.Query);
               end if;

               return;
            end if;
         end if;

         if Seen > Param.Limit then
            if not Param.Prefix.Is_Empty then
               Exchange.Append (Param.Prefix.Query);
            end if;

            if not Param.Ellipsis_Prefix.Is_Empty then
               Exchange.Append (Param.Ellipsis_Prefix.Query);
            end if;

            case Param.Going is
               when Backward =>
                  Ending_Showing_Loop
                    (Iterator.First, Param.Limit - Extra_Items);
               when Forward =>
                  Ending_Showing_Loop
                    (Iterator.Last, Param.Limit - Extra_Items);
            end case;

            if not Param.Ellipsis_Suffix.Is_Empty then
               Exchange.Append (Param.Ellipsis_Suffix.Query);
            end if;

            if not Param.Suffix.Is_Empty then
               Exchange.Append (Param.Suffix.Query);
            end if;

            return;
         end if;
      end if;

      if Param.Limit > 0
        and then (not Param.Ellipsis_Prefix.Is_Empty or else Extra_Items > 0)
      then
         if Seen = 0 then
            for I in Iterator loop
               Seen := Seen + 1;
               exit when Seen > Param.Limit;
            end loop;

            if Seen = 0 then
               if not Param.If_Empty.Is_Empty then
                  Exchange.Append (Param.If_Empty.Query);
               end if;

               return;
            end if;
         end if;

         if not Param.Prefix.Is_Empty then
            Exchange.Append (Param.Prefix.Query);
         end if;


         if Seen > Param.Limit then
            if not Param.Ellipsis_Prefix.Is_Empty then
               Exchange.Append (Param.Ellipsis_Prefix.Query);
            end if;

            Rendered := Extra_Items;
         end if;
      elsif not Param.Prefix.Is_Empty then
         if Seen = 0 then
            for I in Iterator loop
               Seen := Seen + 1;
               exit;
            end loop;
         end if;

         if Seen = 0 then
            if not Param.If_Empty.Is_Empty then
               Exchange.Append (Param.If_Empty.Query);
            end if;

            return;
         else
            Exchange.Append (Param.Prefix.Query);
         end if;
      end if;

      case Param.Going is
         when Forward =>
            for Position in Iterator loop
               Loop_Body (Position, Exit_Loop);
               exit when Exit_Loop;
            end loop;

         when Backward =>
            for Position in reverse Iterator loop
               Loop_Body (Position, Exit_Loop);
               exit when Exit_Loop;
            end loop;
      end case;

      if Rendered = 0 then
         if not Param.If_Empty.Is_Empty then
            Exchange.Append (Param.If_Empty.Query);
         end if;
      else
         if not Param.Suffix.Is_Empty then
            Exchange.Append (Param.Suffix.Query);
         end if;
      end if;
   end Render;

end Natools.Web.List_Templates;
