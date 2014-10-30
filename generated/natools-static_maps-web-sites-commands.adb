with Interfaces; use Interfaces;

package body Natools.Static_Maps.Web.Sites.Commands is

   P : constant array (0 .. 2) of Natural :=
     (3, 7, 12);

   T1 : constant array (0 .. 2) of Unsigned_8 :=
     (14, 23, 24);

   T2 : constant array (0 .. 2) of Unsigned_8 :=
     (16, 13, 19);

   G : constant array (0 .. 24) of Unsigned_8 :=
     (0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 1, 8, 6, 0, 0, 4, 0, 10, 0, 9, 0, 3, 7,
      1, 2);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 25;
         F2 := (F2 + Natural (T2 (K)) * J) mod 25;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 12;
   end Hash;

end Natools.Static_Maps.Web.Sites.Commands;
