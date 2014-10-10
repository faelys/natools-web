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

------------------------------------------------------------------------------
-- Natools.Web.Containres provides common containers for all website-wide   --
-- persistent data.                                                         --
------------------------------------------------------------------------------

with Natools.Constant_Indefinite_Ordered_Maps;
with Natools.S_Expressions.Caches;
with Natools.S_Expressions.Lockable;

package Natools.Web.Containers is

   package Expression_Maps is new Constant_Indefinite_Ordered_Maps
     (S_Expressions.Atom,
      S_Expressions.Caches.Cursor,
      S_Expressions.Less_Than,
      S_Expressions.Caches."=");

   procedure Set_Expressions
     (Map : in out Expression_Maps.Constant_Map;
      Expression_List : in out S_Expressions.Lockable.Descriptor'Class);
      --  (Re)initialize expression database with the given list

   function Get_Expression
     (Map : Expression_Maps.Constant_Map;
      Label : S_Expressions.Atom)
     return S_Expressions.Caches.Cursor;
      --  Return the S-expression associated with the given label.
      --  When label ends with '?', look up without it and
      --  dont report error when not found.

end Natools.Web.Containers;
