with Interfaces; use Interfaces;

package body Natools.Static_Maps.Web.Tags.Commands is

   P : constant array (0 .. 1) of Natural :=
     (4, 11);

   T1 : constant array (0 .. 1) of Unsigned_8 :=
     (1, 32);

   T2 : constant array (0 .. 1) of Unsigned_8 :=
     (29, 8);

   G : constant array (0 .. 32) of Unsigned_8 :=
     (3, 0, 10, 0, 0, 0, 11, 0, 11, 11, 0, 0, 0, 4, 0, 6, 2, 0, 3, 0, 0, 0,
      0, 0, 0, 5, 1, 0, 2, 7, 14, 10, 0);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 33;
         F2 := (F2 + Natural (T2 (K)) * J) mod 33;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 16;
   end Hash;

end Natools.Static_Maps.Web.Tags.Commands;
