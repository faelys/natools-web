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
      package Constructors renames Natools.S_Expressions.Atom_Ref_Constructors;

      use type S_Expressions.Events.Event;
   begin
      case Commands.To_Command (S_Expressions.To_String (Name)) is
         when Commands.Unknown_Command =>
            null;

         when Commands.Backward =>
            State.Going := Backward;

         when Commands.Ellipsis_Prefix =>
            if Arguments.Current_Event = S_Expressions.Events.Add_Atom then
               State.Ellipsis_Prefix
                 := Constructors.Create (Arguments.Current_Atom);
            end if;

         when Commands.Ellipsis_Suffix =>
            if Arguments.Current_Event = S_Expressions.Events.Add_Atom then
               State.Ellipsis_Suffix
                 := Constructors.Create (Arguments.Current_Atom);
            end if;

         when Commands.Forward =>
            State.Going := Forward;

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

         when Commands.Ellipsis_Prefix =>
            State.Ellipsis_Prefix.Reset;

         when Commands.Ellipsis_Suffix =>
            State.Ellipsis_Suffix.Reset;

         when Commands.Forward =>
            State.Going := Forward;

         when Commands.Length_Limit =>
            State.Limit := 0;

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



   -----------------------
   -- Generic Rendering --
   -----------------------

   procedure Render
     (Exchange : in out Exchanges.Exchange;
      Iterator : in Iterators.Reversible_Iterator'Class;
      Param : in Parameters)
   is
      procedure Loop_Body
        (Position : in Iterators.Cursor; Exit_Loop : out Boolean);

      Rendered : Count := 0;

      procedure Loop_Body
        (Position : in Iterators.Cursor; Exit_Loop : out Boolean) is
      begin
         Exit_Loop := False;

         declare
            Template_Copy : S_Expressions.Caches.Cursor := Param.Template;
         begin
            Render (Exchange, Position, Template_Copy);
         end;

         Rendered := Rendered + 1;

         if Param.Limit > 0 and then Rendered >= Param.Limit then
            if not Param.Ellipsis_Suffix.Is_Empty then
               Exchanges.Append
                 (Exchange,
                  Param.Ellipsis_Suffix.Query.Data.all);
            end if;

            Exit_Loop := True;
         end if;
      end Loop_Body;

      Exit_Loop : Boolean;
   begin
      if Param.Limit > 0 and then not Param.Ellipsis_Prefix.Is_Empty then
         declare
            Seen : Count := 0;
         begin
            for I in Iterator loop
               Seen := Seen + 1;
               exit when Seen > Param.Limit;
            end loop;

            if Seen > Param.Limit then
               Exchanges.Append
                 (Exchange,
                  Param.Ellipsis_Prefix.Query.Data.all);
            end if;
         end;
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
   end Render;

end Natools.Web.List_Templates;
