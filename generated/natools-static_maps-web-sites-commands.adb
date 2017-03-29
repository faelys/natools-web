with Interfaces; use Interfaces;

package body Natools.Static_Maps.Web.Sites.Commands is

   P : constant array (0 .. 3) of Natural :=
     (1, 8, 12, 14);

   T1 : constant array (0 .. 3) of Unsigned_8 :=
     (10, 1, 30, 27);

   T2 : constant array (0 .. 3) of Unsigned_8 :=
     (17, 10, 22, 38);

   G : constant array (0 .. 39) of Unsigned_8 :=
     (0, 12, 0, 8, 16, 0, 13, 0, 0, 0, 0, 0, 0, 17, 15, 0, 2, 0, 0, 0, 11,
      4, 14, 0, 12, 0, 3, 0, 0, 1, 13, 0, 16, 0, 6, 1, 0, 0, 13, 0);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 40;
         F2 := (F2 + Natural (T2 (K)) * J) mod 40;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 19;
   end Hash;

end Natools.Static_Maps.Web.Sites.Commands;
