with Interfaces; use Interfaces;

package body Natools.Static_Maps.Web.Tags.Commands is

   P : constant array (0 .. 2) of Natural :=
     (1, 11, 16);

   T1 : constant array (0 .. 2) of Unsigned_8 :=
     (8, 16, 7);

   T2 : constant array (0 .. 2) of Unsigned_8 :=
     (44, 21, 45);

   G : constant array (0 .. 52) of Unsigned_8 :=
     (8, 0, 15, 12, 4, 0, 0, 0, 0, 16, 0, 0, 19, 0, 0, 0, 16, 0, 14, 5, 0,
      0, 0, 4, 0, 0, 0, 9, 0, 0, 6, 0, 23, 0, 3, 2, 17, 0, 20, 0, 0, 24, 0,
      0, 1, 21, 0, 0, 19, 22, 9, 13, 5);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 53;
         F2 := (F2 + Natural (T2 (K)) * J) mod 53;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 26;
   end Hash;

end Natools.Static_Maps.Web.Tags.Commands;
