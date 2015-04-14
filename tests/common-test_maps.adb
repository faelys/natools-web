------------------------------------------------------------------------------
-- Copyright (c) 2014-2015, Natacha Porté                                   --
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
-- Parts of this file were generated with vim in project root directory,    --
-- using the following base:                                                --
-- :r !grep -h function generated/*-t.ads                                   --
-- Transforming base to with list:                                          --
-- :s/^function\(.*\)$/with\1;/                                             --
-- Transforming base to (part of the) returned expression:                  --
-- :s/^function/     and/                                                   --
------------------------------------------------------------------------------

with Natools.Static_Maps.Web.Comments.T;
with Natools.Static_Maps.Web.Error_Pages.T;
with Natools.Static_Maps.Web.Fallback_Render.T;
with Natools.Static_Maps.Web.List_Templates.T;
with Natools.Static_Maps.Web.Simple_Pages.T;
with Natools.Static_Maps.Web.Sites.T;
with Natools.Static_Maps.Web.Tag_Pages.T;
with Natools.Static_Maps.Web.Tags.T;

function Common.Test_Maps return Boolean is
begin
   return Natools.Static_Maps.Web.Comments.T
     and Natools.Static_Maps.Web.Error_Pages.T
     and Natools.Static_Maps.Web.Fallback_Render.T
     and Natools.Static_Maps.Web.List_Templates.T
     and Natools.Static_Maps.Web.Simple_Pages.T
     and Natools.Static_Maps.Web.Sites.T
     and Natools.Static_Maps.Web.Tag_Pages.T
     and Natools.Static_Maps.Web.Tags.T;
end Common.Test_Maps;
