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
-- Natools.Web.Page_Maps provides a general associative map from            --
-- hierarchical keys to page references.                                    --
-- It is based on constant maps, so the mapping must be built in a separate --
-- unsafe object.                                                           --
------------------------------------------------------------------------------

with Ada.Streams;
with Natools.Constant_Indefinite_Ordered_Maps;
with Natools.S_Expressions;
with Natools.Web.Pages;

package Natools.Web.Page_Maps is

   package Raw_Maps is new Constant_Indefinite_Ordered_Maps
     (S_Expressions.Atom, Pages.Page_Ref, Ada.Streams."<", Pages."=");

   procedure Get
     (Container : in Raw_Maps.Constant_Map;
      Path : in S_Expressions.Atom;
      Page : out Pages.Page_Ref;
      Found : out Boolean;
      Extra_Path_First : out S_Expressions.Offset);
      --  Return the page associated with Path or a prefix of Path, provided
      --  the remaining starts with '/' or '?'.

end Natools.Web.Page_Maps;
