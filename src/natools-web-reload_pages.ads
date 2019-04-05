------------------------------------------------------------------------------
-- Copyright (c) 2015-2019, Natacha Porté                                   --
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
-- Natools.Web.Reload_Pages provides a URL endpoint that unconditionally    --
-- reloads the site upon POST request and redirects to the given path.      --
--                                                                          --
-- WARNING: site reload can be a very expensive operation, and no access    --
-- restriction or rate-limiting is implemented at this time, so care should --
-- be taken to ensure this endpoint is only accessible to trusted clients.  --
------------------------------------------------------------------------------

with Natools.S_Expressions;
with Natools.Web.Sites;

private with Natools.S_Expressions.Atom_Refs;

package Natools.Web.Reload_Pages is

   type Reload_Page is new Sites.Page with private;

   overriding procedure Respond
     (Object : in out Reload_Page;
      Exchange : in out Sites.Exchange;
      Extra_Path : in S_Expressions.Atom);


   type Loader is new Sites.Transient_Page_Loader with private;

   overriding function Can_Be_Stored (Object : in Loader) return Boolean
     is (False);

   overriding procedure Load
     (Object : in out Loader;
      Builder : in out Sites.Site_Builder;
      Path : in S_Expressions.Atom);


   function Create (File : in S_Expressions.Atom)
     return Sites.Page_Loader'Class;

private

   type Reload_Page is new Sites.Page with record
      Target : S_Expressions.Atom_Refs.Immutable_Reference;
   end record;

   type Loader is new Sites.Transient_Page_Loader with record
      Target : S_Expressions.Atom_Refs.Immutable_Reference;
   end record;

end Natools.Web.Reload_Pages;
