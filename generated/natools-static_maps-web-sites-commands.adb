with Interfaces; use Interfaces;

package body Natools.Static_Maps.Web.Sites.Commands is

   P : constant array (0 .. 4) of Natural :=
     (1, 6, 9, 12, 14);

   T1 : constant array (0 .. 4) of Unsigned_8 :=
     (20, 2, 21, 1, 4);

   T2 : constant array (0 .. 4) of Unsigned_8 :=
     (28, 15, 21, 6, 7);

   G : constant array (0 .. 32) of Unsigned_8 :=
     (3, 0, 0, 0, 1, 0, 0, 0, 0, 10, 0, 0, 14, 0, 12, 0, 0, 5, 0, 10, 5, 0,
      0, 1, 3, 8, 14, 0, 13, 0, 13, 6, 0);

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

end Natools.Static_Maps.Web.Sites.Commands;
