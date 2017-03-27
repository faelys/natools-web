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

------------------------------------------------------------------------------
-- Natools.Web.ACL provides a simple interface for access control to data.  --
--                                                                          --
-- Currently the backend is responsible for whatever method of user         --
-- authentication (e.g. a random token stored in a cookie), and returns the --
-- current user and the groups to which they belong.                        --
------------------------------------------------------------------------------

with Natools.References;
with Natools.S_Expressions.Lockable;
with Natools.Storage_Pools;
with Natools.Web.Containers;
with Natools.Web.Exchanges;

package Natools.Web.ACL is

   procedure Match
     (Id : in Containers.Identity;
      Expression : in out S_Expressions.Lockable.Descriptor'Class;
      Result : in out Boolean);
      --  Update Result according to whether Id matches the given Expression



   type Backend is interface;

   procedure Authenticate
     (Self : in Backend;
      Exchange : in out Exchanges.Exchange)
     is abstract;
   --  Look up the identity associated with the given exchange
   --  (e.g. using a cookie) and store it there.
   --  Note that it can be called concurrently from several tasks.


   package Backend_Refs is new Natools.References
     (Backend'Class,
      Storage_Pools.Access_In_Default_Pool'Storage_Pool,
      Storage_Pools.Access_In_Default_Pool'Storage_Pool);

end Natools.Web.ACL;
