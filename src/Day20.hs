{-# OPTIONS_GHC -Wno-unused-imports -Wno-missing-signatures -Wno-unused-matches -Wno-dodgy-imports #-} 
{-# LANGUAGE FlexibleContexts, FlexibleInstances, RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving, DerivingVia, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}

module Day20 where

import Control.Lens.Operators
import Control.Lens hiding (uncons)
import Debug.Trace(traceShow, trace)
import Data.Maybe(fromMaybe)
import Data.Bool(bool)
import Data.Function(on, fix)
import Data.Either(fromRight)
import Data.List(unfoldr, intersperse, sort, uncons)
import Data.List.NonEmpty(NonEmpty(..))
import Control.Monad(guard, join)
import Data.Semigroup(Semigroup(..))
import Data.Maybe(fromMaybe, maybe, fromJust)
import Data.Functor.Foldable(hylo, cata, gcata, ListF(..))
import Data.Functor.Compose(Compose(..))
import Data.Tuple(swap)
import Data.Functor.Classes(Show1(..), showsPrec1)
import Control.Applicative(liftA2, Alternative)

import qualified Control.Foldl as F
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MPC
import qualified Text.Megaparsec.Char.Lexer as MPCL
import qualified Parsing as P
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.IntMap.Strict as IM

import Data.Coerce(Coercible, coerce)
import Text.Show.Deriving(deriveShow, deriveShow1)
import Control.Monad.State.Strict(StateT, MonadState, evalStateT, execStateT, get, execState, State(..), state, runState, runStateT)
import Control.Monad.Reader(ReaderT, MonadReader, runReaderT, ask)
-- import Control.Lens(use, deepOf, makeLenses, makePrisms, view, ix, preview, preuse, Traversal, Lens', Ixed(..), Index, IxValue, Iso', iso, _2, over, _Wrapping')
import Control.Monad.Trans(lift)
import Grid as G
import Data.Functor.Foldable(Fix(..), unfix, cata)
import Data.Foldable(fold, toList)
import Utility((<$$>))
import qualified Utility as U
import Vector
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE

day20Input :: String
day20Input = "SENEESWSWSSWSWSWNWSWWSESEESESSWSWSWWSSWSWSWWSSWNNWSSWWWSSSEESSENEEEENENEENNWNN(NENNESSSS(WNSE|)ESWSSEESWWSSENEENNEESESSSEEEEEESSSSSWSWNWNNNEE(SWSEWNEN|)NWWWWSSSSWWWWSWWWSWWWSWWSWSEENEESSSEENWNEENWN(EESESSSWW(NENSWS|)SWWWWSSWWNWWNNN(ESSENE(SS|N(E(N|S)|W))|WWNWNWSWWNNNWNNWSSWWSWWWSWSEENESSEENWNENESSSSEN(NNN|ESSSWSEESSWWWSWNNE(EE|NNWN(EE|NWSSWSE(E|SSWNWNWSSWSWSEEESEEEEESSENENWNN(EEENWNEESESSSWSSSSSENESSWWWWWWWNWWWS(WWNENEEEEN(WWNN(ESNW|)WWWWSSS(ENEN(ESNW|)W|SSWNNWNNWNWNNE(NWWNENNWSWSSSSSWNNWNENWWNWWNENEES(E(SWEN|)NE(EENWNENWNWWNENESESESE(SEEE(NWWEES|)S(ENSW|)WS(WWSS(ENSW|)W(SEWN|)NNN(N|EE)|E)|NNWNNEE(SWEN|)ENEE(SWEN|)EENWNNENWWNNESENNWWNWWNNWNNEENEEENWNEESSENNNENNWSWWWNWSWSSE(NEEEWWWS|)SWWWWS(EE|WNNENWNWWWSSWSESSWSWWSWNWSWWSWWWSEEESWWSESWWNWWSWWWWSSWWWNNWSSSEESWSESWSWWW(NNESENNWNNNNW(NNENNNNW(SSS|NENNW(NENEESENNESENNESSSESSESSENESENNENNWSWNNNWNWW(SES(ESS(WN|SE)|W)|NNWNEENESSS(WNSE|)ESESSENNEENNWNENWNEENWWWSSSWW(NENNNNWSWWWSWSS(ENENEESW(ENWWSWENEESW|)|WWWNENWNNWSWWW(SEEESWWWSEEESEEESS(E|WWN(E|WSS(SWNNNNWWSESSSW(S|NN)|EE)))|NNNESE(SWEN|)NENNNEENNNWWSWWSW(SSENE(NEEN|SSWW)|NNNNENENWNEESESSW(N|WS(SWNSEN|)EEENEEENWWNENWNNENWNWWWWWNNENENNNNEESWSEESWWSESS(WNWSWENESE|)EENN(WSNE|)ESEENNESEEENWWNWWNWWW(SEES(WW|S)|NEEENWWNWNNNWWWNNEEENWNWNENWWNENEEEESSWSW(NN(W|E)|SESESENENEENEENNWNENEENWWWSWSSWWNENNENWNWNWWWNWNEENNWNEENWNNNWWWSESWWNNWSWNNWSSSWW(SESSW(N|SSEEESSSWSESSE(SWSSWWNW(NNEE(S(W|S)|NWWNENNW(NEESNWWS|)S)|SSSEESE(SSWWW(NE(NWES|)E|SSESSSSW(SSES(ENN(NNNESEENE(S(E|SS)|NNWWS(E|S|WNW(N(E|W)|S)))|W)|SW(N|SSSSEESWS(W(SSS|N)|EEENN(WSNE|)ESSS(W|SS))))|NNN))|NE(SE|NWWW)))|EEEEENEN(ESSWSSSEE(SWWSWN|EE)|NWWS(WS(E|WNNWSW(SEWN|)NNNESE(ESNW|)NNNWNWNEEE(S(SEWN|)W|N(ENNSSW|)WWWW(NEWS|)SSW(NN|S(EESEWNWW|)W)))|E))))|NENNN(WSSNNE|)EEEES(W|ENESEEEESSEEEENESENENN(WWWWS(EEE|WNW(SSEEWWNN|)WWWW)|ESEEN(EEEEESEESWSSWNNWSWWNW(NEE(S|E)|WWSEESSSENNESE(SSSWWWN(EENSWW|)WWSWWNNNWWN(WSWWWN(W(WNNSSE|)SSSEESWSESEESSW(NW(WNWW(W|NN(ESNW|)N)|S)|SEENNNESESWSESSESEEEESWWSEEESSWWN(WSWNNNWSSSSEEESWWWWSSEEESWSWWSSWWSEEENEESWSSSWNNWWSWNWWNNWNENNNWSSWSSW(SES(E(SEESEEE(N|SESENEEENNWN(WSSEWNNE|)EEEENNWWNEENNNESESWSEEENESEESEEESWSSESSWSWNWSWWWSWWWNNENWWN(WWSSSSEE(SSWNWSSSWWNWSWWNNWNWSWNWWNWSSWSWWNNE(ENN(WSWWSSW(SS(W|E(N|SS(WNSE|)SSEENNNW(NEESENNENESEESWWWSEESWSWWN(E|WSSESSSEENWNNEES(SSEENEEESSWW(NEWS|)WSSWNWSWSSEESWSWWS(EEENESSWSEENNNENEEESSSEESWSWNWNWSW(NNEENW|SWWSWW(NEWS|)SEESWWSESSWNWSSW(NN|SESENNEEEE(NWNW(S|NEESE(S|NNESEENEEESW(SEE(SWSEWNEN|)NNNNWNNNNESSSESSS(ENNENENWW(NENNNNENNEEENEENNWWWNEENWNNWWNENNEESENEEESESEESEENEESWSSEEN(W|ENEESESENNEEESENNNNESEESWWSSEEESSWNWWSWSEE(N|ESSESSEENNENNNENNENEEEEENWNNESEESSW(N|SSWWN(WSWSEESESENN(ENESESWWSEEENESSESSWWSSWSWNNENNN(ESEWNW|)WWWWWSSEESE(SSSWWSSEEN(ESSSSEEEEESWSWSEENEENWNNWWWWNENNNW(NENNEEEENNNENENESEEENWNNNWWSES(WWWNENNNWNENWWWSWSWWNWWNEENWNWWS(E|S(SSSS(EEEEEEE(SSWNWSW(NWSWNW|SSEE(SWSSEN|EN(E|WW)))|ENWN(WSWWWWNWS(NESEEEWWWNWS|)|N))|W)|WNNNWNENNEEENNEENNESESWSSSWNW(NEWS|)SSESS(SS(ENNESEENEENEESWSW(SEEEN(NNNEEESSWW(NEWS|)SSEEEESWSSWNNWSWSESSWW(N(E|NWNWSWNNEEE(E|S))|SESSESWSWNWSSWSSWSWWSWNWSSESENESENN(W|ESENN(ENN(WSNE|)EEN(NESSSWSSW(NN|SW(N|SEENENNENEENESSSSWNNWSSSSEESSEEENENNE(NNWNENNWWS(WNW(SSESWSS(WW|ENE(E|NN|SSWWSE))|NNESENENWNWNWWNEEENWWWNNWSW(WWSEEESSWSSW(SEEE(NWNEESE(WNWWSEWNEESE|)|SWWSWN)|WNENN(WWSEWNEE|)E)|NNENESENESSS(WNSE|)EESSS(W|E(S(SSSWENNN|)W|NNNNNNWNENWNENWNENWNENWNNE(NNNNWWSWNNNWWNEEEENE(NNWSWSWNNNNES(EENNW(NWSWWNNE(ENEE(NWWWSWNNNEEES(ENNWWWWWNENENENNNNWSWWSSS(ENENWESWSW|)WNNNNNNN(WSWNWSWNWWWSEESEESESSE(NNNWESSS|)SWSESWWNWNENWN(WN(WSSWNWWSWWNWSSEEESEEEE(N(WWWNSEEE|)N|SWSESSSSSSSSEESSSSSSSEENNE(EENNNNWWWNENWW(SSSSS(EEENNWSWN(SENESSNNWSWN|)|SS)|NENWNENEE(NWWNN(WSSWS(E|W(NNN(ESNW|)N|SESWSESW(ENWNENSWSESW|)))|E(NEWS|)S)|SSS(WNNSSE|)SS))|SSSEESSWSWSEENESSWSSEE(SSSSWNNWSWNWWW(SEESSESW(SEEENN(W(NWES|)S|ESSSSSEN(SWNNNNSSSSEN|))|WNNWSSWW(EENNESNWSSWW|))|NEENNE(SSEEWWNN|)NWWWWSES(ENSW|)WSWWNNNNWSWWNENWNENESES(W|ENEESWS(WSSSNNNE|)EENNEN(E|WNWSWWWNNE(SEWN|)NWNNNWSWNWNWNEEE(ES(E(N|SSS)|WW)|NWWNWWWWWSESESSWWSESWSWNNNNN(ESEWNW|)WSWNWSWWWWNEENWWWSSSEEEESENESE(N|SWWWSESEE(NWES|)SESSES(WWNNWSWSESWS(WNNNWNNE(SENEWSWN|)NWWWNNESENNWWWWNWSSSSSWSWNWSSSWWWWSEESSSENNNESEENENWN(WSSNNE|)EESSEESEE(SS(ENSW|)WNWSSWNWWN(EN(ESNW|)W|WWWSEESEESWWSSESWWWSSWNNWNWSSESWSSSE(ENN(WSNE|)EEES(ENNWN(WSNE|)EENNE(N(NEWS|)WW|SSE(S|N))|WW)|SSWNW(SSSE(SW(WNSE|)SS|N)|NNW(SWWEEN|)NNNE(NNNEENN(ESENESSSWWNE(WSEENNSSWWNE|)|WSWWWSWNWWWWNWWSWWWWNEENWNENNNWNWNEENNWWNNNEEESWS(WNSE|)ESSSS(W|SESEENESSESSW(SEEEENNNE(NWWNWW(SESESSWN(SENNWNSESSWN|)|NNWSWW(WNEENENNNEESWSSENENENWNWWNNNWWWNEENENWWNWNEESEENESEEEESESWWWSESWSSEEEENWW(NNESEEESSSE(NNNEE(SWEN|)NWNENWNEENWNWWNNNENESESESS(WNWNWS|ENNESSSWSSW(NN|WSEEEENWNEENWNNNNWNEEESSSW(NN|SESSENNNNNESEENENE(NWWSW(WNNE(S|NNNWSWNWNENEN(WWWWSE(SSWNWNNWSWNWSWSSWSESENNEE(NWWEES|)SSSEEENE(NWWW(SEWN|)N|E(SWS(ESNW|)WWWWWS(ESWENW|)WNWN(EENNSSWW|)WWS(WS(WSESSSWNNWNNNENNWNWSWWSWSWNWWWSESSSWWNENWWNNNNWWSWSWWWNWWWWWSESWWWNWWWNWSWSWWNNE(NE(S|NWNNN(WWWSWNWSWW(SSWSEEEENWWNEEEEE(NWES|)SSWW(NEWS|)SWSWNWWSSWSSESESSEENNNNWW(SESSNNWN|)(W|N(N|EEESE(SWSSEEESEESESENNEENNWSWNWNWSW(SEESNWWN|)NWN(EENESEE(NWES|)S(ENESSSSEESEEEENNWSWWNWNNW(NEESSE(EEENNWN(EESESSW(N|SWSSENENEENN(W(S|NENW(NENSWS|)W)|ESSEES(ENENNEN(WNSE|)EESSENNN(W|E(N(E|W)|SSSSSE(N|SS(SENNEE(SWSSS(WNWS|ENN)|E)|WNWWNENWWN(WSS(ESWENW|)W|N)))))|WWWWSWSESWSE(E(E|N)|SWWNWNENWWSSWWSWWWWSEESEESWWSWNNWSSSSESWSWWNNNNE(NNNNWSWWSSSSE(NNENWESWSS|)SWSWNWSWSWNWSWW(SEESENESENNESEE(SENEEESSWSSENENNNN(NNESEE(S(EE|WSESWW(SEEESESWWNWWSWSWWSSSESWWWWSEESSESW(SEE(S|EENWWNEENENEESSSEEENENESESSS(ENNNNWNN(EES(W|E(SWSEENNEE(SWSSWENNEN|)N|N))|W(N|WS(WSWSWNNN(E(S|E)|WWWWW(S(WWSEEWWNEE|)E|NNWNEESSENNN(W|ES(ENSW|)SS|N)))|E)))|WWW(S|WWWW(N(NW(NENSWS|)S|EEEEEENW(ESWWWWEEEENW|))|S))))|WNWN(WSWWNWSWWWNWNW(NEEES(SENNESEE(NE(S|NENENWWSWSW(W|NNENWN(NNEE(NWES|)ESSS(WNWNEWSESE|)ENNE(N(NEWS|)W|SSSSSW)|W)|S))|S)|W)|SWNWWWSSEE(NWES|)EESWSSEE(SWWSWWSSENESEENN(WSNE|)ES(ENSW|)SSSWS(EENEEWWSWW|)WNN(E|WWSWWSWSWSSEEN(W|ESSS(WNWWW(NWWWNNEN(WWWSESWS(E|W(SE|NWW))|EE(SSW(W|N)|ENNE(E(NNWWWWNEENWWNWWWN(EEEESEEEES(WSWNSENE|)ENNWN(WWNWSSEE(WWNNESNWSSEE|)|E)|WW(SW(S(W|SES(WSNE|)ENN(EESS(WNSE|)E(SSEE(SWWEEN|)N(EE|W)|NN)|W|N))|N)|NN))|E)|S)))|SEES(SSSSS(WWW(S|N(EENSWW|)WSWN(NEWS|)WSW(SEWN|)N)|E)|WW))|ENNE(SSSWSS|NWNN(WSWENE|)E(SESNWN|)N))))|N(N(ESNW|)N|W)))|E))|NN))|NNW(N(WSWWEENE|)ENENNE(NWWS(S|WW)|ESEE(NWES|)S(WWWNSEEE|)ESE(SWW(SEESWSWN(SENENWESWSWN|)|N)|ENNW(W|S)))|S))|W)|N(W|N))|WWW(S|NWNEESEEENWNW(NENNWNENWNWNENWWWSSSE(ESSS(SSWNNWWSWW(SWNWSSSENE(SSWS(E|WSW(SSE(EE|N)|W(NEN(NNWWWEEESS|)E|WW)))|EEN(ENSW|)W)|NNE(NWNWNEESE(SEESWW|NNWNWWNNWSWNWNENEEEN(ESE(NNWNENENWWS(NEESWSNENWWS|)|ESSWWS(E(S|EE(NN|EEEEESWWSES(SEENWNENESEN(ESSWWSESWSSSWWW(NENE(NWWSNEES|)S|SESEEN(EENNN(WSSNNE|)NNENE(S|EN(NNEEESWWSEEENEES(ENSW|)WSWWWW|WW))|W))|NWWNWW)|W)))|WNN(EE|WW)))|WWWW(WWS(E(E|SWSSE(ESES(WWNWESEE|)E(EE|N)|N))|WNWNNNWWW(SSSENE(NWES|)S|WW))|N)))|S))|E)|NN)|S)))|SSS))|S)))|WSS(E|WNNN(WSNE|)E))|S)|SS)|W)|WSS(W|E))|NN(N|W))))|N(E|WW))|ESES(W|ENEN(ESSWSEEENNESSS(EENWNNESEENN(ESSESWWWSEEESENENNNESENN(WWWS(WNSE|)SS|ESSENESSSW(NWWWEEES|)S(SS|EENNNESEEENNN(EESWSS(EEN(W|NEEN(ESE(NEWS|)SWWSW(SEE(S(WSNE|)E|N)|N)|WW))|SWWSWNWS(NESENEWSWNWS|))|WSW(SEWN|)WN(E|W(WW|S)))))|W(S|WWWWWSS(NNEEEEWWWWSS|)))|WWWW(SEEES|WWNE))|WW))))|S)|E)|E)|E))|E)|EESWS(ESEE(NNN(WSSNNE|)EESSW(SEENEWSWWN|)N|S)|W)))|S)|SSEEE(ESWWSESSWNWNWWWWW(SEESWS(W(N|SWS(E|WNN(WSSWSNENNE|)E))|EEEE(NWWNEWSEES|)EEE(SWEN|)NNN)|NEEE(N|EE))|NWN(WSNE|)E)))))|SWWWWN(EENNSSWW|)WWSESWSSENESE(NN(W|EEESWW)|SSWNWSWNWWW(NEEN(NNNWNNNNNN(EE|WWSESSW(NW|SESW))|W)|WSEESEN(SWNWWNSEESEN|))))|W)|S))|SSE(SWEN|)NE(NWES|)S)|WWNE(NWWSSW(S|NNN)|E))))|SS))))|NNW(NNWW(SESWENWN|)W(W|NE(EE|NNW(NEWS|)S))|S))|E)|SESSSSSSS(WWNENNN(SSSWSEWNENNN|)|EENWNEEES(ENNNE(SSEEEWWWNN|)NWWWWSEESWWWNNNENNNE(NNWSWNNEENNN(NW(NNN(W|E)|SSSW(WSSW(NWES|)SESSE(S|N)|NN))|ESEESWSW(S(EENSWW|)S|N))|SSS)|W)|S)))))))|N(NNNE(S|N)|W))))|E)|E)|ESES(SWNSEN|)EENEE(NWWW(W|S)|SWSSE(SSSW(SS(ENSW|)W(N|W)|NN)|N)))|WW)|S(S|W))|S)|S)|S)|SSWS(WNSE|)E)|S)))))|E)|SSSWSESWSESSWSSE(SSWNWNWSWNNWNEEE(SWEN|)N(WNWSWWWSSE(N|SWWNNWSWWWSWWNWWNNENWNEEEEESSS(ENEENWNW(NNESENEE(SES(SWWN(W|E|N)|EEE(NWWEES|)S)|NWWNN(NN|WSSWWSSWNNN(WSWSS(ENSW|)WWW(NENN(WSW(NWNWW(SEWN|)NNNNE(SSS|EEEENWNNESE(NENWWEESWS|)S)|S)|E(ENEWSW|)SS)|SSSWW(NEWS|)WWWWSWWNNNN(EE(SWS(S|E)|E)|WNNNWWSESWSWNWWNNNWWNNENWWSSSSSWWWNNENWNNNN(WSSSWWWNWNEENNN(NWSSWNNWN(W(NWWWN(EENNESE(S(W|S)|E)|WSWNN(N|WSSWNWSSEEE(SSWSSS(ENNESNWSSW|)WNWWWWNNNWSWWNWWW(NEEENNWNEESENEE(NWWWNWSWWNN(ESNW|)NWSWSESW(WNNWNSESSE|)SSENESS(W|E)|SESWW(N|WWS(S|EEEESSES(WWNNSSEE|)ENN(E|W))))|SWSEENESESEE(NWES|)SWWSWWSWNWSWNNEN(ESEENW|WN(N|WSWW(NEWS|)SWSSSEENE(SSWWSEEENNEESWSSSSEENESSSSS(WSWNWW(NNEN(WNWNN(ESNW|)WSSWNW(NEWS|)S(WN|SE)|EESSW(W|N))|WSES(WSWWWSESENEE(SSW(N|SWW(N(E|WNWWN(NESNWS|)W(W|SSEESE))|SEESWWSESSW(N|S(W|EEEESSSENNEENENNEENESSENNENE(NNW(NEWS|)SW(N|WS(WNWSW(N|WWWSSWNNNW(NEESNWWS|)SSWSSE(EEE(SWEN|)ENWNEEE(WWWSESNWNEEE|)|N))|E))|SESWWSEEESWS(EENENEEE(SWWEEN|)NNWWNWSS(W(S|WNN(E(NEEE(NWNSES|)S|S)|W))|EE)|WSWWNN(WWSSS(ENNSSW|)WNN(NNEWSS|)WSSWNWSS(WWW(NNNWSNESSS|)S|EE(SW(SEWN|)W|E))|E(S|E))))))))|N)|E(N|EE)))|ENESENENENWNNEENWNE(NWNWWNW(WWSWSSWS(S|EEEEE(SWWSEESWWSEE(WWNEENSWWSEE|)|ENWWWN(WSNE|)E(NWES|)EE))|NEESENN(EESWENWW|)W)|EES(E|SW(SWWEEN|)N)))|NNWSWS))))|EEEE)|E))|SSSE(S(EE|SSWWW(WSSSSSEESENESEESWSSESWSWNWNNE(S|NWWSSWSWWSSW(SESS(W(W|N)|ENEEESEEESSESWWS(WWNNE(N(WWNW|ES)|S)|EEEENWNEENENENENNEESWSSENEESEENESSENESESSSENNNNWNEENWWWSWNN(EEEEEEEEE(SSWSWNWN(WSSESSESENN(W|ESEENWN(W|EN(WNEWSE|)EEEENESSSSSWNNNWSWW(NEWS|)SSEE(NWES|)SWSSWNNWWSSSWSSESWWSSWNNWNEENNNWWWWN(WSSESE(EN(ESNW|)W|SWSSSSEE(SSEEEEESESESWWWSSSSSEENWNNEN(EENENESSENENEENENNNWWWNWSWSWNNWNWSSWS(WWNENWW(NEENNEENN(ESSES(WWWSSNNEEE|)EENENWW(S|WNNN(WSNE|)NESSES(W|EESEEEENENENNE(NWWSWWNENNE(EESWWEENWW|)NWWSWWWWSW(NNN(N|EEEE(SWWWEEEN|)N)|SESWSEES(W|EEE(SWWEEN|)N(WNNWW(NEEE|SES)|E(S|E))))|SSSSSSWNW(NENSWS|)SWWSWWW(W(NENEE(SWEN|)EE|WW(W|S))|SEEEENESESSSWWSW(NNNEESW(ENWWSSNNEESW|)|SSSESSSSWSSEEEE(SSWWN(WWSESWWSEESWS(WWN(NWSSWNNWNWSSWWNNE(NNNNWNENWW(SSSSWWWSWNNEN(EESWENWW|)WWSWSESWSWNWWSWSSES(EEENESENEES(W|EE(EEEENSWWWW|)NWNNE(N(ENNSSW|)WWSWS(WW(NEWS|)WWSWN(W|N)|E)|S))|WWWWWNENENE(SSWENN|)NNE(S|ENWNWSWSWWNNWSWWSEESEE(E|SWWS(SSWWNENNNWSSWNWWSESWS(EENSWW|)WNWSWNWWS(WNWSWWNNWNWNNEENEEEESWWWSEEEESES(EENWNWNNW(NENESSSESEEENNWSWNNW(S|NNENESS(EES(ENNEEEEENWNWWNNNENEENWNWSWSWSSWNNNENWNNEENENNENWWNENEN(ESS(W|SESSW(WSSES(E(NNWESS|)S(EESWWSESWSESSW(SSS(WWW(S(S|E)|WW)|ENEN(NNESEESWSES(ENNNNNNN(ESEWNW|)WSSW(SEWN|)NWSWNNEE(WWSSENSWNNEE|)|WW(WS(E(SWSEWNEN|)E|W)|NN))|W))|WN(NW(WSE|NE)|E))|W)|WW(N|W(W|S)))|N))|WNENWN(WSWSSS(ENNSSW|)WWNENWWNWWNEN(WW(SSWN(WSWSSESEENEN(ESESWSSESWWNN(N|WSWNWWNW(N|SSESE(ESWWW(SEESENESENEESSE(NNENE(ENWNNWW(W|SESWSWWWWN(SEEEENSWWWWN|))|S)|SWWSESWSESWWSES(WWNWSWWSEE(SWWWWWWWNENNEEE(EENNWWWNEEENNEEN(E(ENSW|)SSSSWW(NNES|SE)|WWWWWWN(WWSSWWWWW(SEESESSESE(NNWNN(W|ESESSE(NNNN(W(S|N)|EES(ENSW|)W)|EEE))|SWSWNNWSSSESWSS(ENE(S|EEN(WWNSEE|)EES(ENEWSW|)W)|WSESWSSSWS(EENN(ESESWENWNW|)N|WNWNNNENE(SSSWNSENNN|)NWWWNWWWSESWSSSSWS(EENNNESSSE(E|NNNNN(E|W(SWEN|)N))|WNNWSSWNNNENEE(S(S|W)|NNNWSWS(E|WNW(SS(W(SEWN|)N|E)|NNNEN(EESWS(SWNSEN|)EENNENNESENEESWSSENESESS(WNW(WWWN(NES|WS)|S)|ENNNWNWNEE(S|NENWW(SWW|NE)))|NWSWNWWWNWNEESEENWN(E|WWN(WSNE|)E)))))))))|NWN(EESEEENWN(E(E|NNNN)|W(S|WW))|WW))|E(N|EE)))|SWWSEE)|E)|ENE(NNNN|ESE(N|S(WW(SWNSEN|)N|EE)))))|N)|N)))|WW(W|S))|N)|N)|E(N|ES(W|E(SW|NES))))|E))|WW)|W))|S)|WWNWSWNW(WNWESE|)SS)|E)|E))))|N(EEN(N|WW|ESENENESENE(SESSWNWWWSWSW(N|SSENENESEE(NWNWESES|)SWSWW(NEWS|)S(WWNEWSEE|)SEEN(ENSW|)W)|N(WW|N)))|W))|S)|E)|EEENWNEN(ESSSNNNW|)W)|E)|NNWSW(W|NNEENNNWW(SESWENWN|)NN(WSNE|)NESE(SWEN|)NNNNNNW(ESSSSSNNNNNW|))))))))|WW(NN|S(W|E)))|SSWWNE)|ES(SE(EN(WNNSSE|)ESE(NEN(W|EESWSW)|S)|S)|W))|W)|NNW(N|S)))|NEN(NW(S|N(N|E))|ESS(W|ENESE(NNEEWWSS|)S)))))|EE)|NWWWWW)|NNWSSSWW(NENWNENN(E(SENSWN|)N|WSWNWN(WNW(NNESNWSS|)SWWN(E|WWWNWSSW(NNNENN(ESSNNW|)WWWSE(SW(SEWN|)WNNN(WWSESWW(SEEWWN|)NN|E)|E)|S(W|ESWSSSW(SWS(EENEENNW(NEEENEN(ESENESESESW(SSENSWNN|)WNWSWNWSSWW(NEWS|)SE(E|SWWS(W(WSEEWWNE|)N|E))|WWSWNNEE(WWSSENSWNNEE|))|S)|WNWWNE(NW(N|WSW(SEWN|)N)|EE))|N))))|E))|S))))|WNENNEENENNWW(NNNN(W(S|WW)|N)|S(WS(WSSNNE|)E|E))))|NNE(SENSWN|)N))|N))|EEE)|ESSSSW)|EENEN(EESENE(SSWS(WWW(NE(E|N)|WWSSE(SSSWENNN|)N)|SSS(W|ENNNES(SSSSSWNN(SSENNNSSSWNN|)|EN(ES|NW))))|NN(NWSSWWNN(WSSNNE|)NE(SS|N(NEWS|)W)|E))|W))))|EE)))|S)|WW(S|NENWWSSW(ENNEESNWWSSW|))))|E)|N))))|WW)|W)))|W)|W)|S)|WNWNWW(SE|NEE))))|S)|SW(NWES|)S(SS|E))|W)|NN(ESNW|)WW)|W)|E))))|S)|S)|W)))|SWWWSS(WWS(E|WN(WSS(WWWSSWS(WNNWNE(ESNW|)NWN(WWSWSEE(SWWSWW(NENNNNN(ES(S|EEENWNWS(NESESWENWNWS|))|W(N|SSSS))|SEES(WWW|E(SWSEWNEN|)NEE(NWWEES|)S(W|EE)))|N)|E)|EE(NN|E))|E)|N))|EEEN(E|WW)))))|WNNNNWWSWWSS(ENEENSWWSW|)WWNWSS(E|WNNW(W|NEENN(WSWNSENE|)EEES(WS(WNSE|)S|ENNNWNENN(WWWWSSESE(S(WWNSEE|)E|NN(W|E))|EESSSW(SSENEE(N(W|E(EE|S))|S(SSSEWNNN|)W)|NN))))|SS))|W))|SS)))|N(N|W))|EEEEN(ESS(E(N|EES(W|ES(WSNE|)EE(NWNENWWS(NEESWSNENWWS|)|S)))|WW)|W))|S)|NNW(N|S))|ENENEESEEN(EESWSWWSS(ENEE(E|N)|WNNW(SSSWENNN|)(W|N))|WN(WWWWSNEEEE|)E)))|N)|W)|NNWNWNW(SSESEWNWNN|)N(N|WWW|EEEE(SSWNWESENN|)ENN(WSWENE|)EEN(W|EEE(NNWW(SEWN|)NEN(ESNW|)W|SWWSSW(NWSNES|)SESSS(W(NN|SS)|ENN(EE|NNN))))))|E))|EE)|EEE(ESWS(EENNSSWW|)S|N(N|W)))|N))|W))))))))))|SEES(W|E)))|S))|SSSS)|SESESSWSSSEESESSSENNENNEESWSEEENWNEEENWNWNWWWSWNWNNNN(WSSSW(NNWESS|)SESS(WNWSNESE|)E(N|S(S|W)|EEENESE(WNWSWWEENESE|))|ENWNENESSSENNNNENEEESESWSSSWNNNNWSSSW(NN|SSSES(WWWNENWWSS(NNEESWENWWSS|)|ESEES(ESWSSESESWWNWNWSSWWSWWN(WSSWWWSESENEESWSWWSWWSS(WWNENWNENNNNW(NNN(NNNNN(E|N)|EESS(WNSE|)E(N|SWSSSESW(ENWNNNSSSESW|)))|SSS)|EEEN(ESENESEEEENNENWNWNWSWNW(SSS(W(WW|N)|ENESS(ENNSSW|)W)|N(W|EEEN(E(SSESESSESS(WWNEWSEE|)EEEEENNNWWSWW(SEEENSWWWN|)NWNNNW(N(WSNE|)EESSEENNE(SESWS(WW(W|S)|EESESSS(WNNSSE|)EENWNNE(ES(SESWENWN|)W|NW(W|NNN)))|NWW(SS|WN(NWN(ENWNEE(NENWNEENE(NENEENWWWNENNWSWNWNNWWN(NNNESSSENNEEENEN(WWSWWEENEE|)EEESWWSESSSSENNNEE(NNW(SWEN|)NNWWW(W|NE(NNNNWSSS(NNNESSNNWSSS|)|EE))|SWSESESWW(N|SWWWN(EE|W(NNE(S|NNW(SWNWWSESWSEEN(SWWNENSWSEEN|)|N))|SSEES(ENEE(SWEN|)EEN(WW|E(S|NWNNNNWSS(NNESSSNNNWSS|)))|SS)))))|WSSESSWNW(SSESWSEESES(WWNWW(SSE(N|S(WWNWN(WNSE|)E|SENNESS(NNWSSWENNESS|)))|NN)|ENNE(S|NWN(NW(SS(WNSE|)S|NN)|E)|E))|WN(WW(N|WWWWS(SWWSNEEN|)E)|E)))|SSSS(ENNESS|W(NN|W)))|S)|W)|EE)))|S)|N)|W)))|WW))|E)|W)))))))|S)|W)|SEE(S(W|SS)|E)))|EESEE(SWWWNSEEEN|)NWN(NE(NN(ENSW|)W(W|S)|S)|W))|EE)|WSWWWNW(W(W|N)|S)))))))|W(S|W))|WSSS(WW(NENWESWS|)S(SWNNWSSWNWN(WSNE|)E|E)|E))"

day20Test = "WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))"

squareList = [('N', G.SquareN),('E', G.SquareE),('W', G.SquareW),('S', G.SquareS)] 

squareMap :: M.Map Char G.SquareDirection
squareMap = M.fromList squareList

data Path d a = 
    Sequence d a
    | Alternatives (NE.NonEmpty a) a
    | EndPath

instance (Functor (Path d)) where
    fmap f (Sequence d a) = Sequence d (f a)
    fmap f (Alternatives alts a) = Alternatives (f <$> alts) (f a)
    fmap _ _ = EndPath

instance (Show d) => (Show1 (Path d)) where
    -- liftShowsPrec :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> Path d a -> ShowS
--  showsPrec1 _ Nil s = ""
    liftShowsPrec f g x (Sequence d a) = liftA2 (++) (showsPrec x d) (f (x+1) a) 
    liftShowsPrec f g x (Alternatives as a) = liftA2 (++) (g $ NE.toList as) (f (x+1) a) 
    liftShowsPrec _ _ _ EndPath = (++ "X")

inverseSquareMap = M.fromList (Data.Tuple.swap <$> squareList)

dirParser :: P.Parser [G.SquareDirection]
dirParser = do
    asString <- MP.takeWhile1P (Just "Direction") (flip M.member squareMap)
    MP.label "Directions" $ pure $ (squareMap M.!) <$> asString 

toNonEmpty :: [a] -> NE.NonEmpty a
toNonEmpty (x:xs) = x :| xs
toNonEmpty _ = error "List should not be empty"

pathParser :: P.Parser d -> P.Parser a -> P.Parser (Path d a)
pathParser dir inner = (MP.try alt) MP.<|> seq MP.<|> (pure EndPath) where
    seq = Sequence <$> dir <*> inner
    alt = MP.label "Alt" $ Alternatives <$> MP.between (MP.single '(') (MP.single ')') alt2 <*> inner
    alt2 = toNonEmpty <$> MP.sepBy inner (MP.single '|') 



{-
-- https://stackoverflow.com/questions/54030077/lifting-a-lens-traversal-to-fix/54030559#54030559
directions :: Applicative f => LensLike f p q a b -> LensLike f (Path a p) (Path b q) a b
directions pq a2fb (Path l) = Path <$> traverse f l where
    f (Directions d) = Directions <$> a2fb d
    f (Alt p) = Alt <$> (traverse . pq) a2fb p


traverseFix :: Functor m => (forall x y. LensLike m x y a b -> LensLike m (f x) (g y) a b) 
        -> LensLike m (Fix f) (Fix g) a b
traverseFix traverseF = traverseFix' where
    traverseFix' a2fb (Fix h) = Fix <$> traverseF traverseFix' a2fb h

directions' :: forall a b . Traversal (Fix (Path a)) (Fix (Path b)) a b
directions' = traverseFix directions
display :: Fix (Path [G.SquareDirection]) -> String
display (Fix (Path l)) = foldMap i l
    where i (Directions d) = (inverseSquareMap M.!) <$> d
          i (Alt as) = "(" ++ (join $ intersperse "|" (display <$> as)) ++ ")"

unPath (Path x) = x

-- deriveShow1 ''Path
deriveShow1 ''PathComponent
-}

type Route = Fix (Path [G.SquareDirection])

fixPathParser :: P.Parser Route
fixPathParser = fix $ fmap Fix . (pathParser dirParser)

day20FixPath = MP.parse fixPathParser "" day20Input

-- Stored x first, then y
newtype Grid a = Grid (IM.IntMap (IM.IntMap a)) deriving (Show)

deriveShow1 ''Grid

{-# INLINE coerceIso #-}
coerceIso :: (Coercible a b) => Iso' a b
coerceIso = iso coerce coerce

instance Wrapped (Grid a) where
    type Unwrapped (Grid a) = IM.IntMap (IM.IntMap a)
    {-# INLINE _Wrapped' #-}
    _Wrapped' = coerceIso

type instance Index (Grid a) = Vector2D Int
type instance IxValue (Grid a) = a

instance Ixed (Grid a) where
    {-# INLINE ix #-}
    ix (Vector2D x y) = (_Wrapping' Grid) . (ix x) . (ix y)

instance U.AtWithDefault (Grid a) where
    atEmpty = Grid $ IM.empty
    atWithD d (Vector2D x y) = (_Wrapping' Grid) . U.atD2 d x y where

instance (Semigroup a) => Semigroup (Grid a) where
    (<>) (Grid x) (Grid y) = Grid $ IM.unionWith (IM.unionWith (<>)) x y 
    sconcat ls = Grid $ IM.unionsWith (IM.unionWith (<>)) $ extract <$> ls
        where extract (Grid m) = m

instance (Semigroup a) => Monoid (Grid a) where
    mappend = (<>)
    mempty = Grid $ mempty

data DoorStructure a = DoorStructure {
    _finalPositions :: NE.NonEmpty (Square Int),
    _doors :: Grid a
} deriving (Show)

makeLenses 'DoorStructure

translate :: Square Int -> Grid a -> Grid a
translate (Square (Vector2D x y)) = over (_Wrapping' Grid) $ IM.mapKeysMonotonic (+x) . fmap (IM.mapKeysMonotonic (+y))

translateDS :: Square Int -> DoorStructure a -> DoorStructure a
translateDS v = liftA2 DoorStructure fp ds where
    fp = (v <>) <$$> (view finalPositions)
    ds = (translate v) <$> (view doors)

parallelCombine :: (Semigroup a) => NE.NonEmpty (DoorStructure a) -> DoorStructure a
parallelCombine dss = DoorStructure fp ds where
    fp = NE.nub $ (view finalPositions) =<< dss
    ds = foldMap (view doors) dss

serialCombine :: (Semigroup a, Show a) => DoorStructure a -> DoorStructure a -> DoorStructure a
serialCombine x y = result where
    ys = parallelCombine $ translateDS <$> (view finalPositions x) <*> pure y
    addXDoors = ((view doors x) <>)
    result = doors %~ addXDoors $ ys
    dg = (show x) ++ "\n + " ++ (show y) ++ "\n = " ++ (show result)
{-}
class (Foldable g) => Groupable g where
    groupByKey :: (Eq k) => (v -> k) -> g v -> g (NonEmpty v)

groupByKeyStep :: (Eq k) => (v -> k) -> v -> [v] -> (NonEmpty v, [v])
groupByKeyStep getKey v l = (v :| vs, ls) where
    (vs, ls) = span ((== (getKey v)) . getKey) l

instance Groupable [] where
    groupByKey getKey = unfoldr f where
        f = (uncurry $ groupByKeyStep getKey) <$$> uncons    

instance Groupable NonEmpty where
    groupByKey getKey (v :| vs) = v1 :| groupByKey getKey ls where
        (v1, ls) = groupByKeyStep getKey v vs
-}
--        groupByKey' :: (Eq k) => (v -> k) -> NonEmpty v -> NonEmpty (NonEmpty v)

-- groupByKey :: (Eq k) => (v -> k) -> [v] -> [NonEmpty v]

toIntMap :: (Foldable g) => (v -> Int) -> g v -> IM.IntMap (NonEmpty v)
toIntMap getKey = foldMap (IM.singleton <$> getKey . NE.head <*> id) . NE.groupWith getKey

toGridFromSortedByVector :: (Semigroup m, Foldable g) => (v -> Vector2D Int) -> (v -> m) -> g v -> Grid m
toGridFromSortedByVector toV toM = Grid . ((fmap . fmap) (sconcat . fmap toM))  . fmap (toIntMap (_y . toV)) . toIntMap (_x . toV)
    
processMove :: (Direction d, Vector v d) => d -> v -> ([(v, d)], v)
processMove d p = (doors, p')
    where p' = move d p
          doors = [(p, d),(p', (opposite d))]

origin = Square $ Vector2D 0 0
          
toDoorStructure :: [G.SquareDirection] -> DoorStructure (S.Set G.SquareDirection)
toDoorStructure path = f $ process path origin where
    process = runState . fmap join . traverse (state . processMove) 
    f (pairs, p) = DoorStructure (p :| []) (toG pairs) 
    toG = toGridFromSortedByVector (coerce . fst) (S.singleton . snd) . sort

path2DoorStructure :: (ds ~ DoorStructure (S.Set G.SquareDirection)) => (Path [G.SquareDirection] ds) -> ds
path2DoorStructure (Sequence d a) = serialCombine (toDoorStructure d) a
path2DoorStructure (Alternatives as a) = serialCombine (parallelCombine as) a
path2DoorStructure EndPath = toDoorStructure []

path2Display :: (Path [G.SquareDirection] String) -> String
path2Display (Sequence d a) = (fmap (inverseSquareMap M.!) d) ++ a
path2Display (Alternatives as a) = "(" ++ ((join . intersperse "|" . toList) as) ++ ")" ++ a
path2Display EndPath = ""

-- adjacency :: P.ParseResult (DoorStructure (S.Set SquareDirection))
adjacency = cata path2DoorStructure <$> day20FixPath 

data FloodFillState a = FloodFillState {
    _currentNodes :: S.Set a,
    _allNodes :: S.Set a -- allNodes should be a superset of current Nodes
}

makeLenses 'FloodFillState

floodFillStep :: (Ord a, MonadState (FloodFillState a) m, Alternative m) => (a -> [a]) -> m (S.Set a)
floodFillStep neighbours = do
    cn <- use currentNodes
    an <- use allNodes
    let nextNodes = S.fromList $ foldMap (filter (not . flip S.member an) . neighbours) cn
    allNodes %= S.union nextNodes
    currentNodes .= nextNodes
    nextNodes <$ guard (not $ S.null nextNodes)

floodFillDistances :: (Ord a) => (a -> [a]) -> a -> NE.NonEmpty (S.Set a)
floodFillDistances neighbours s = initial :| fromMaybe [] steps where
    initial = (S.singleton s)
    start = FloodFillState initial initial
    steps = evalStateT (MP.many (floodFillStep neighbours)) start


gridNeighbours :: Grid (S.Set G.SquareDirection) -> Square Int -> [Square Int]
gridNeighbours g v = traverse move directions v where
    directions :: [G.SquareDirection]
    directions = foldMap S.toList $ preview (ix (coerce v)) g

computeFurthest s = floodFillDistances <$> (gridNeighbours . _doors <$> adj s) <*> (pure origin)

adj s = cata path2DoorStructure <$> MP.parse fixPathParser "" s

day20 = floodFillDistances <$> (gridNeighbours . _doors <$> adjacency) <*> (pure origin)
day20a = (subtract 1) . length <$> day20
day20b = sum <$> length <$$> drop 1000 . toList <$> day20

-- foldDown :: (Semigroup s) => (s, [s]) -> s
-- foldDown (x, ys) = sconcat (x :| ys)
{-}
fd :: (Monoid m) => (a -> m) -> Path m m -> m
fd f (Path cs) = foldMap pc cs where
    pc (Directions x) = f x
    pc (Alt as) = mconcat as
-}

{-}
newPath :: Fix (Path d) -> (Maybe d, [Fix (Path d)])
newPath = newPath' . unPath . unfix where
    newPath' ((Dir d):l) = (Just d, (Fix . Path) <$> [l])
    newPath' ((Alt alts):l) = (Nothing, (coerce . (++ l) . coerce) <$> alts) where
    newPath' _ = (Nothing, [])

toFixPath = fromRight (Fix $ Path []) . MP.parse fixPathParser "" 
testA =  toFixPath "ENWWW(NEEE|SSE(EE|N))"
test = fromRight (Fix $ Path []) $ MP.parse fixPathParser "" day20Test


type Result a = ([(Square Int, G.SquareDirection)], a)

takeStep :: Day20BuildState -> Result [Day20BuildState]
takeStep (Day20BuildState p pa) = _2 %~ newState $ doorsPosition where
    (d, pa') = newPath pa
    doorsPosition :: Result (Square Int)
    doorsPosition = fromMaybe ([], p) (processMove p <$> d)
    newState :: Square Int -> [Day20BuildState]
    newState p' = Day20BuildState p' <$> pa'

toAdjacency = hylo (foldDown . getCompose) (Compose . takeStep) . start
adjacency = toAdjacency <$> day20FixPath

testAdjacency = toAdjacency <$> MP.parse fixPathParser "" "W"
-}
    {-
groupByKey getKey (v:vs) = groupByKeyInternal getKey v vs 
groupByKey _ _ = []

withKey :: (Eq k) => (v -> k) -> NonEmpty v -> (k, NonEmpty v)
withKey getKey = ((,) <$> getKey . NE.head <*> id)

uniq :: (Eq a) => [a] -> [a]
uniq = fmap NE.head . groupByKey id

toGrid :: (Ord v, Ord a, Coercible v (Vector2D Int)) => [(v, a)] -> Grid (S.Set a)
toGrid pairs = undefined
-}{-

-- deriving via (IM.IntMap (IM.IntMap a)) instance U.AtWithDefault (Grid a) 
-}{-
gridDefault :: (Coercible v (Vector2D Int)) => a -> v -> Lens' (Grid a) a
gridDefault def v = U.atD2 def (x :: Int) (y :: Int) 
    where (Vector2D x y) = coerce v
    -}