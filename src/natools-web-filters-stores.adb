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

with Natools.S_Expressions.Interpreter_Loop;
with Natools.S_Expressions.Special_Descriptors;

package body Natools.Web.Filters.Stores is

   procedure Add_Filter
     (Filters : in out Filter_Maps.Unsafe_Maps.Map;
      Constructors : in Constructor_Maps.Constant_Map;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class);
      --  Build a new filter and add it to Filters

   procedure Update is new S_Expressions.Interpreter_Loop
     (Filter_Maps.Unsafe_Maps.Map, Constructor_Maps.Constant_Map, Add_Filter);


   -----------------------------
   -- Local Helper Suprograms --
   -----------------------------

   procedure Add_Filter
     (Filters : in out Filter_Maps.Unsafe_Maps.Map;
      Constructors : in Constructor_Maps.Constant_Map;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class)
   is
      use type S_Expressions.Events.Event;
      Position : Constructor_Maps.Cursor;
   begin
      if Arguments.Current_Event /= S_Expressions.Events.Add_Atom then
         Filters.Exclude (Name);
         return;
      end if;

      Position := Constructors.Find (Arguments.Current_Atom);

      if not Constructor_Maps.Has_Element (Position) then
         Log (Severities.Error, "Unable to find filter type """
           & S_Expressions.To_String (Arguments.Current_Atom) & '"');
         return;
      end if;

      Arguments.Next;

      begin
         declare
            New_Filter : constant Filter'Class
              := Constructor_Maps.Element (Position).all (Arguments);
         begin
            Filters.Include (Name, New_Filter);
         end;
      exception
         when No_Filter => null;
      end;
   end Add_Filter;



   ----------------------
   -- Public Interface --
   ----------------------

   function Duplicate (Source : in Store) return Store is
   begin
      return
        (Constructors => Source.Constructors,
         Filters => Filter_Maps.Empty_Constant_Map);
   end Duplicate;


   function Get_Filter
     (Container : in Store;
      Name : in S_Expressions.Atom)
     return Filter'Class is
   begin
      Try_Filter :
      declare
         Position : constant Filter_Maps.Cursor
           := Container.Filters.Find (Name);
      begin
         if Filter_Maps.Has_Element (Position) then
            return Filter_Maps.Element (Position);
         end if;
      end Try_Filter;

      Try_Constructor :
      declare
         Position : constant Constructor_Maps.Cursor
           := Container.Constructors.Find (Name);
      begin
         if Constructor_Maps.Has_Element (Position) then
            return Constructor_Maps.Element (Position).all
              (S_Expressions.Special_Descriptors.Empty_Descriptor);
         end if;
      end Try_Constructor;

      raise No_Filter with "No filter with given Name found in container";
   end Get_Filter;


   procedure Populate
     (Container : in out Store;
      Expression : in out S_Expressions.Lockable.Descriptor'Class)
   is
      Map : Filter_Maps.Unsafe_Maps.Map := Container.Filters.To_Unsafe_Map;
   begin
      Update (Expression, Map, Container.Constructors);
      Container.Filters := Filter_Maps.Create (Map);
   end Populate;


   procedure Register
     (Container : in out Store;
      Name : in S_Expressions.Atom;
      Callback : in Constructor) is
   begin
      Container.Constructors
        := Container.Constructors.Include (Name, Callback);
   end Register;


   procedure Register
     (Container : in out Store;
      Name : in S_Expressions.Atom;
      Filter : in Filters.Filter'Class) is
   begin
      Container.Filters := Container.Filters.Include (Name, Filter);
   end Register;

end Natools.Web.Filters.Stores;
