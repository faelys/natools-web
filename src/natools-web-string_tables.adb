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

with Ada.Containers.Doubly_Linked_Lists;

package body Natools.Web.String_Tables is

   package Row_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Containers.Atom_Array_Refs.Immutable_Reference,
      Containers.Atom_Array_Refs."=");


   ------------------
   -- Constructors --
   ------------------

   not overriding function Create
     (Expression : in out S_Expressions.Lockable.Descriptor'Class)
     return String_Table is
   begin
      return String_Table'(Ref => Create (Expression));
   end Create;


   function Create
     (Expression : in out S_Expressions.Lockable.Descriptor'Class)
     return Table_References.Immutable_Reference
   is
      List : Row_Lists.List;
      Event : S_Expressions.Events.Event := Expression.Current_Event;
      Lock : S_Expressions.Lockable.Lock_State;
   begin
      Read_Expression :
      loop
         case Event is
            when S_Expressions.Events.Close_List
              | S_Expressions.Events.End_Of_Input
              | S_Expressions.Events.Error
            =>
               exit Read_Expression;

            when S_Expressions.Events.Add_Atom => null;

            when S_Expressions.Events.Open_List =>
               Expression.Lock (Lock);
               begin
                  Expression.Next (Event);

                  if Event in S_Expressions.Events.Add_Atom
                            | S_Expressions.Events.Open_List
                  then
                     List.Append (Containers.Create (Expression));
                  end if;

                  Expression.Unlock (Lock);
               exception
                  when others =>
                     Expression.Unlock (Lock, False);
                     raise;
               end;
         end case;

         Expression.Next (Event);
      end loop Read_Expression;

      if Row_Lists.Is_Empty (List) then
         return Table_References.Null_Immutable_Reference;
      end if;

      Build_Table :
      declare
         Data : constant Table_References.Data_Access
           := new Table (1 .. S_Expressions.Count (Row_Lists.Length (List)));
         Ref : constant Table_References.Immutable_Reference
           := Table_References.Create (Data);
         Cursor : Row_Lists.Cursor := Row_Lists.First (List);
      begin
         for I in Data.all'Range loop
            Data (I) := Row_Lists.Element (Cursor);
            Row_Lists.Next (Cursor);
         end loop;

         pragma Assert (not Row_Lists.Has_Element (Cursor));

         return Ref;
      end Build_Table;
   end Create;



   ---------------
   -- Renderers --
   ---------------

   overriding procedure Render
     (Exchange : in out Sites.Exchange;
      Object : in String_Table;
      Expression : in out S_Expressions.Lockable.Descriptor'Class)
   is
      pragma Unreferenced (Exchange);
      pragma Unreferenced (Object);
      pragma Unreferenced (Expression);
   begin
      raise Program_Error with "Not implemented yet";
   end Render;

end Natools.Web.String_Tables;
