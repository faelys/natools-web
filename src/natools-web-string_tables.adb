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

package body Natools.Web.String_Tables is

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
      pragma Unreferenced (Expression);
   begin
      raise Program_Error with "Not implemented yet";
      return Table_References.Null_Immutable_Reference;
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
