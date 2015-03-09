
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

with Natools.S_Expressions.Caches;

procedure Natools.Web.Render_Default
  (Exchange : in out Natools.Web.Sites.Exchange;
   Object : in Natools.Web.Tags.Visible'Class)
is
   Cache : S_Expressions.Caches.Reference;
   Cursor : S_Expressions.Caches.Cursor;
begin
   Cache.Open_List;
   Cache.Append_Atom (S_Expressions.To_Atom ("render"));
   Cache.Append_Atom (Exchange.Site.Default_Template);
   Cache.Close_List;
   Cursor := Cache.First;
   Tags.Render (Exchange, Object, Cursor);
end Natools.Web.Render_Default;