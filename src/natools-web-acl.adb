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

with Natools.S_Expressions.Conditionals.Generic_Evaluate;
with Natools.Static_Maps.Web.ACL;

package body Natools.Web.ACL is

   Logged_Command : constant S_Expressions.Atom
     := (1 => Character'Pos ('l'),
         2 => Character'Pos ('o'),
         3 => Character'Pos ('g'),
         4 => Character'Pos ('g'),
         5 => Character'Pos ('e'),
         6 => Character'Pos ('d'));

   function Parametric_Evaluate
     (Id : in Containers.Identity;
      Name : in Natools.S_Expressions.Atom;
      Arguments : in out Natools.S_Expressions.Lockable.Descriptor'Class)
     return Boolean;

   function Simple_Evaluate
     (Id : in Containers.Identity;
      Name : in Natools.S_Expressions.Atom)
     return Boolean;


   function Evaluate_Match
     is new Natools.S_Expressions.Conditionals.Generic_Evaluate
     (Containers.Identity, Parametric_Evaluate, Simple_Evaluate);


   ------------------------------
   -- Local Helper Subprograms --
   ------------------------------

   function Parametric_Evaluate
     (Id : in Containers.Identity;
      Name : in Natools.S_Expressions.Atom;
      Arguments : in out Natools.S_Expressions.Lockable.Descriptor'Class)
     return Boolean
   is
      package Commands renames Natools.Static_Maps.Web.ACL;
      use type S_Expressions.Atom;
      S_Name : constant String := S_Expressions.To_String (Name);
      Event : S_Expressions.Events.Event := Arguments.Current_Event;
   begin
      case Commands.To_Command (S_Name) is
         when Commands.Unknown_Command =>
            Log (Severities.Error, "Unknown identity condition """
              & S_Name & '"');
            return False;

         when Commands.Is_In_All_Groups =>
            while Event in S_Expressions.Events.Add_Atom loop
               if not Containers.Contains (Id.Groups, Arguments.Current_Atom)
               then
                  return False;
               end if;
               Arguments.Next (Event);
            end loop;
            return True;

         when Commands.Is_In_Any_Group =>
            while Event in S_Expressions.Events.Add_Atom loop
               if Containers.Contains (Id.Groups, Arguments.Current_Atom) then
                  return True;
               end if;
               Arguments.Next (Event);
            end loop;
            return False;

         when Commands.Is_User =>
            while Event in S_Expressions.Events.Add_Atom loop
               if Id.User.Query = Arguments.Current_Atom then
                  return True;
               end if;
               Arguments.Next (Event);
            end loop;
            return False;

      end case;
   end Parametric_Evaluate;


   function Simple_Evaluate
     (Id : in Containers.Identity;
      Name : in Natools.S_Expressions.Atom)
     return Boolean
   is
      use type S_Expressions.Atom;
   begin
      if Name = Logged_Command then
         return not Id.User.Is_Empty;
      elsif Id.User.Is_Empty then
         return False;
      elsif Name = Id.User.Query then
         return True;
      else
         return Containers.Contains (Id.Groups, Name);
      end if;
   end Simple_Evaluate;



   ----------------------
   -- Public Interface --
   ----------------------

   procedure Match
     (Id : in Containers.Identity;
      Expression : in out S_Expressions.Lockable.Descriptor'Class;
      Result : in out Boolean) is
   begin
      if Expression.Current_Event
        in S_Expressions.Events.Add_Atom | S_Expressions.Events.Open_List
      then
         Result := Evaluate_Match (Id, Expression);
      end if;
   end Match;

end Natools.Web.ACL;
