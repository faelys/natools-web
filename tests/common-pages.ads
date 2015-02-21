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
-- Common.Pages provides a special web page that provides test utilities.   --
------------------------------------------------------------------------------

with Natools.S_Expressions;

package Common.Pages is

   package S_Expressions renames Natools.S_Expressions;
   package Sites renames Natools.Web.Sites;


   type Page is new Sites.Page with private;

   overriding procedure Respond
     (Object : in out Page;
      Exchange : in out Sites.Exchange;
      Extra_Path : in S_Expressions.Atom);


   type Loader is new Sites.Page_Loader with private;

   overriding procedure Load
     (Object : in out Loader;
      Builder : in out Sites.Site_Builder;
      Path : in S_Expressions.Atom);

   function Create (File : in S_Expressions.Atom)
     return Sites.Page_Loader'Class;


   type Sleep_Update is new Sites.Updates.Site_Update with private;

   overriding procedure Update
     (Self : in Sleep_Update;
      Site : in out Sites.Site);

   procedure Queue_Sleep (Site : in Sites.Site; Amount : in Duration);


private

   type Page is new Natools.Web.Sites.Page with null record;

   type Loader is new Sites.Page_Loader with null record;

   type Sleep_Update is new Sites.Updates.Site_Update with record
      Amount : Duration;
   end record;

end Common.Pages;
