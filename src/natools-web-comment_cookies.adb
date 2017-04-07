------------------------------------------------------------------------------
-- Copyright (c) 2017, Natacha Porté                                        --
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

package body Natools.Web.Comment_Cookies is

   function Create (A : S_Expressions.Atom)
     return S_Expressions.Atom_Refs.Immutable_Reference
     renames S_Expressions.Atom_Ref_Constructors.Create;

   procedure Set_Atom
     (Info : in out Comment_Info;
      Context : in Meaningless_Type;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class);


   procedure Initialize is new S_Expressions.Interpreter_Loop
     (Comment_Info, Meaningless_Type, Set_Atom);


   ------------------------------
   -- Local Helper Subprograms --
   ------------------------------

   procedure Set_Atom
     (Info : in out Comment_Info;
      Context : in Meaningless_Type;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class)
   is
      pragma Unreferenced (Context);
      use type S_Expressions.Events.Event;
      Kind : Atom_Kind;
   begin
      if Arguments.Current_Event /= S_Expressions.Events.Add_Atom then
         return;
      end if;

      declare
         S_Name : constant String := S_Expressions.To_String (Name);
      begin
         Kind := Atom_Kind'Value (S_Name);
      exception
         when Constraint_Error =>
            Log (Severities.Error, "Unknown comment atom kind """
              & S_Name & '"');
      end;

      Info.Refs (Kind) := Create (Arguments.Current_Atom);
   end Set_Atom;



   ----------------------
   -- Public Interface --
   ----------------------

   function Create
     (Name : in S_Expressions.Atom_Refs.Immutable_Reference;
      Mail : in S_Expressions.Atom_Refs.Immutable_Reference;
      Link : in S_Expressions.Atom_Refs.Immutable_Reference;
      Filter : in S_Expressions.Atom_Refs.Immutable_Reference)
     return Comment_Info is
   begin
      return (Refs =>
        (Comment_Cookies.Name => Name,
         Comment_Cookies.Mail => Mail,
         Comment_Cookies.Link => Link,
         Comment_Cookies.Filter => Filter));
   end Create;


   function Create
     (Expression : in out S_Expressions.Lockable.Descriptor'Class)
     return Comment_Info
   is
      use type S_Expressions.Events.Event;
      Result : Comment_Info := Null_Info;
      Event : S_Expressions.Events.Event;
   begin
      case Expression.Current_Event is
         when S_Expressions.Events.Add_Atom =>
            for Kind in Result.Refs'Range loop
               Result.Refs (Kind) := Create (Expression.Current_Atom);
               Expression.Next (Event);
               exit when Event /= S_Expressions.Events.Add_Atom;
            end loop;

         when S_Expressions.Events.Open_List =>
            Initialize (Expression, Result, Meaningless_Value);

         when S_Expressions.Events.Close_List
           | S_Expressions.Events.End_Of_Input
           | S_Expressions.Events.Error
         =>
            null;
      end case;

      return Result;
   end Create;

end Natools.Web.Comment_Cookies;
