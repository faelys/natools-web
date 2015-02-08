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

------------------------------------------------------------------------------
-- Natools.Web.Sites.Updates provides types representing an update to a     --
-- site object.                                                             --
------------------------------------------------------------------------------

package Natools.Web.Sites.Updates is

   type Site_Update is interface;

   procedure Update (Self : in Site_Update; Object : in out Site) is abstract;
      --  Actually perform the update


   type Reloader is new Site_Update with private;

   overriding procedure Update (Self : in Reloader; Object : in out Site);
      --  Reload site data


private

   type Reloader is new Site_Update with null record;

end Natools.Web.Sites.Updates;
