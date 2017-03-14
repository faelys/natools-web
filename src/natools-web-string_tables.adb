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

with Natools.S_Expressions.Interpreter_Loop;
with Natools.Web.List_Templates;

package body Natools.Web.String_Tables is

   procedure Append
     (Exchange : in out Sites.Exchange;
      Context : in Containers.Atom_Array_Refs.Immutable_Reference;
      Data : in S_Expressions.Atom);

   procedure Append_Table_Node
     (Map : in out Table_Maps.Unsafe_Maps.Map;
      Context : in Meaningless_Type;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class);

   procedure Execute
     (Exchange : in out Sites.Exchange;
      Row : in Containers.Atom_Array_Refs.Immutable_Reference;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class);

   procedure Render
     (Exchange : in out Sites.Exchange;
      Table_Map : in Table_Maps.Constant_Map;
      Name : in S_Expressions.Atom;
      Expression : in out S_Expressions.Lockable.Descriptor'Class);


   procedure Append_Table is new S_Expressions.Interpreter_Loop
     (Table_Maps.Unsafe_Maps.Map, Meaningless_Type, Append_Table_Node);

   procedure Render_Row is new S_Expressions.Interpreter_Loop
     (Sites.Exchange, Containers.Atom_Array_Refs.Immutable_Reference,
      Execute, Append);

   procedure Render_Table is new List_Templates.Render
     (Cursor, Iterator_Interfaces);


   ------------------------------
   -- Local Helper Subprograms --
   ------------------------------

   procedure Append_Table_Node
     (Map : in out Table_Maps.Unsafe_Maps.Map;
      Context : in Meaningless_Type;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class)
   is
      pragma Unreferenced (Context);
      Table_Ref : constant Containers.Atom_Table_Refs.Immutable_Reference
        := Containers.Create (Arguments);
   begin
      if not Containers.Atom_Table_Refs.Is_Empty (Table_Ref) then
         Table_Maps.Unsafe_Maps.Include (Map, Name, Table_Ref);
      end if;
   end Append_Table_Node;



   ------------------------
   -- Renderer Fragments --
   ------------------------

   procedure Append
     (Exchange : in out Sites.Exchange;
      Context : in Containers.Atom_Array_Refs.Immutable_Reference;
      Data : in S_Expressions.Atom)
   is
      pragma Unreferenced (Context);
   begin
      Exchange.Append (Data);
   end Append;


   procedure Execute
     (Exchange : in out Sites.Exchange;
      Row : in Containers.Atom_Array_Refs.Immutable_Reference;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class)
   is
      pragma Unreferenced (Arguments);

      Column : S_Expressions.Offset;
      S_Name : constant String := S_Expressions.To_String (Name);
   begin
      To_Column :
      begin
         if S_Name'Length > 6
           and then S_Name (S_Name'First .. S_Name'First + 5) = "value-"
         then
            Column := S_Expressions.Offset'Value
              (S_Name (S_Name'First + 6 .. S_Name'Last));
         else
            Column := S_Expressions.Offset'Value (S_Name);
         end if;
      exception
         when Constraint_Error =>
            Log (Severities.Error, "Invalid row command """ & S_Name & '"');
            return;
      end To_Column;

      declare
         Atoms : constant Containers.Atom_Array_Refs.Accessor := Row.Query;
      begin
         if Column in Atoms.Data'Range then
            Exchange.Append (Atoms (Column).Query);
         else
            Log
              (Severities.Error,
               "Column"
               & S_Expressions.Offset'Image (Column)
               & " not in row range"
               & S_Expressions.Offset'Image (Atoms.Data'First)
               & " .."
               & S_Expressions.Offset'Image (Atoms.Data'Last));
         end if;
      end;
   end Execute;



   -------------------------------
   -- Table Iterator Primitives --
   -------------------------------

   overriding function First (Object : in Table_Iterator)
     return Cursor is
   begin
      return (Ref => Object.Ref, Index => Object.Ref.Query.Data'First);
   end First;


   overriding function Last (Object : in Table_Iterator)
     return Cursor is
   begin
      return (Ref => Object.Ref, Index => Object.Ref.Query.Data'Last);
   end Last;


   overriding function Next
     (Object : in Table_Iterator;
      Position : in Cursor)
     return Cursor
   is
      pragma Unreferenced (Object);
   begin
      return (Ref => Position.Ref, Index => Position.Index + 1);
   end Next;


   overriding function Previous
     (Object : in Table_Iterator;
      Position : in Cursor)
     return Cursor
   is
      pragma Unreferenced (Object);
   begin
      return (Ref => Position.Ref, Index => Position.Index - 1);
   end Previous;



   ------------------
   -- Constructors --
   ------------------

   not overriding function Create
     (Expression : in out S_Expressions.Lockable.Descriptor'Class)
     return String_Table is
   begin
      return String_Table'(Ref => Containers.Create (Expression));
   end Create;


   not overriding function Create
     (Expression : in out S_Expressions.Lockable.Descriptor'Class)
     return String_Table_Map
   is
      Map : Table_Maps.Unsafe_Maps.Map;
   begin
      Append_Table (Expression, Map, Meaningless_Value);
      return (Map => Table_Maps.Create (Map));
   end Create;



   ---------------
   -- Renderers --
   ---------------

   overriding procedure Render
     (Exchange : in out Sites.Exchange;
      Object : in String_Table;
      Expression : in out S_Expressions.Lockable.Descriptor'Class) is
   begin
      Render_Table
        (Exchange,
         Table_Iterator'(Ref => Object.Ref),
         List_Templates.Read_Parameters (Expression));
   end Render;


   procedure Render
     (Exchange : in out Sites.Exchange;
      Position : in Cursor;
      Expression : in out S_Expressions.Lockable.Descriptor'Class) is
   begin
      Render_Row
        (Expression,
         Exchange,
         Position.Ref.Query.Data (Position.Index));
   end Render;


   procedure Render
     (Exchange : in out Sites.Exchange;
      Table_Map : in Table_Maps.Constant_Map;
      Name : in S_Expressions.Atom;
      Expression : in out S_Expressions.Lockable.Descriptor'Class)
   is
      Cursor : constant Table_Maps.Cursor := Table_Maps.Find (Table_Map, Name);
   begin
      if Table_Maps.Has_Element (Cursor) then
         Render_Table
           (Exchange,
            Table_Iterator'(Ref => Table_Maps.Element (Cursor)),
            List_Templates.Read_Parameters (Expression));
      else
         Log (Severities.Error, "Unable to find string map """
           & S_Expressions.To_String (Name) & '"');
      end if;
   end Render;


   overriding procedure Render
     (Exchange : in out Sites.Exchange;
      Object : in String_Table_Map;
      Expression : in out S_Expressions.Lockable.Descriptor'Class)
   is
      use type S_Expressions.Events.Event;
      Event : S_Expressions.Events.Event;
      Lock : S_Expressions.Lockable.Lock_State;
   begin
      case Expression.Current_Event is
         when S_Expressions.Events.Add_Atom =>
            declare
               Name : constant S_Expressions.Atom := Expression.Current_Atom;
            begin
               Expression.Next;
               Render (Exchange, Object.Map, Name, Expression);
            end;

         when S_Expressions.Events.Open_List =>
            loop
               Expression.Lock (Lock);
               Expression.Next (Event);

               if Event = S_Expressions.Events.Add_Atom then
                  declare
                     Name : constant S_Expressions.Atom
                       := Expression.Current_Atom;
                  begin
                     Expression.Next;
                     Render (Exchange, Object.Map, Name, Expression);
                  end;
               end if;

               Expression.Unlock (Lock);
               Expression.Next (Event);
               exit when Event /= S_Expressions.Events.Open_List;
            end loop;

         when S_Expressions.Events.Close_List
           | S_Expressions.Events.Error
           | S_Expressions.Events.End_Of_Input
         =>
            null;
      end case;
   end Render;

end Natools.Web.String_Tables;
