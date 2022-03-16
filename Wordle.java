import javalib.funworld.*;
import javalib.worldimages.*;
import tester.Tester;
import java.awt.Color;
import java.util.Random;

// stores the world state
class Wordle extends World {
  IList<String> dictionary;
  int numGuesses;
  //cannot have length larger than num Guesses
  IList<IList<String>> guesses; 
  IList<String> secretWord;
  IList<String> currentGuess;
  IList<String> lastInputedGuess;

  //the constructor for a random game
  Wordle(IList<String> dictionary, int numGuesses) {
    this(dictionary, numGuesses, new Random());
  }

  // the constructor for testing with a given random
  Wordle(IList<String> dictionary, int numGuesses, Random rand) {
    this(dictionary, numGuesses, new Empty<IList<String>>(), 
        (new WordList()).split(dictionary.get(rand.nextInt(dictionary.size())), 1, 0), 
        new Empty<String>(), new Empty<String>());
  }

  // the constructor for assigning all of the fields
  Wordle(IList<String> dictionary, int numGuesses, IList<IList<String>> guesses, 
      IList<String> secretWord, IList<String> currentGuess, IList<String> lastInputedGuess) {
    this.dictionary = dictionary;
    this.numGuesses = numGuesses;
    this.guesses = guesses;
    this.secretWord = secretWord;
    this.currentGuess = currentGuess;
    this.lastInputedGuess = lastInputedGuess;
  }


  // draws the current world state.
  public WorldScene makeScene() {
    return new WorldScene(300, 300).placeImageXY(this.draw(),100,150);
  }

  // returns an updated world after a key is pressed.
  public World onKeyEvent(String key) {
    // creates a IList<String> of all of the letters in the alphabet.
    IList<String> alphabeticLetters = 
        (new WordList()).split("abcdefghijklmnopqrstuvwxyz", 1, 0);

    if (alphabeticLetters.contains(key, new StringEquality()) 
        && currentGuess.size() < secretWord.size()) {
      // is the key a letter in the alphabet AND is the size of the currentGuess < the size of the
      // secret word?

      // push the upper case version of the key to the back of the currentGuesses.
      IList<String> newCurrentGuess = currentGuess.pushBack(key.toUpperCase());

      // return the new Wordle
      return new Wordle(this.dictionary, this.numGuesses, this.guesses,
          this.secretWord, newCurrentGuess, this.lastInputedGuess);

    } else if (key.equals("backspace")) {
      // the user types the "delete" or "backspace" key

      // removes the last letter in currentGuess (if there exists one)
      IList<String> newCurrentGuess = currentGuess.removeLast();

      // return the new Wordle
      return new Wordle(this.dictionary, this.numGuesses, this.guesses,
          this.secretWord, newCurrentGuess, this.lastInputedGuess);

    } else if (key.equals("enter") 
        && (this.currentGuess.size() == this.secretWord.size())
        && (this.dictionary.contains(currentGuess.foldr(new StringAppend(), ""), 
            new StringEquality()))) {
      // the user hits the "enter" key AND the size of the current guess = the size of the secret 
      // word and the currentGuess is in the dictionary.

      // the last inputted guess is the current guess.
      IList<String> lastInputtedGuess_ = this.currentGuess;

      // resets the current guess
      IList<String> newCurrentGuess = new Empty<String>();

      int newNumGuesses = this.numGuesses - 1; 
      // num guesses can't be 0, game would've already ended

      // the new guesses 
      IList<IList<String>> newGuesses = new Cons<IList<String>>(
          this.currentGuess, this.guesses);

      return new Wordle(this.dictionary, newNumGuesses, newGuesses, this.secretWord, 
          newCurrentGuess, lastInputtedGuess_);
    }
    return this;
  }

  // should the world end?
  // ** we currently have this set to false because we would rather soft lock the game
  // by disabling user input, than have the last frame of the game suddenly cut off. **
  public boolean shouldWorldEnd() {
    return this.lastInputedGuess.sameList(this.secretWord, new StringEquality()) 
        || (numGuesses <= 0);
  }

  // creates the last scene
  public WorldScene lastScene(String msg) {
    return this.makeScene();
  }

  // draws the Wordle board
  public WorldImage draw() {
    Util util = new Util(secretWord.size());

    // we split the wordle board into three sections:
    // the rowsOfGuesses IList<WorldImage> represents the words that have *already*
    // been guessed
    IList<WorldImage> rowsOfGuesses = guesses.map(new DrawRow(secretWord));
    // the rowOfCurrentGuess WorldImage represents the row that is currently
    // being manipulated by the user (as they type in their next word).
    WorldImage rowOfCurrentGuess = util.completeRowCurrentGuess(currentGuess);
    // the rowsOfDisabledSquares WorldImage represents the rest of the grid
    IList<WorldImage> rowsOfDisabledSquares;

    // the following code is necessary because the Cons only takes numbers > 0 in
    // its default * number of times variant.
    if (numGuesses - 1 > 0) {
      rowsOfDisabledSquares = 
          new Cons<WorldImage>(util.drawRowOfDisabledSquares(), numGuesses - 1);
    } else {
      rowsOfDisabledSquares = 
          new Empty<WorldImage>();
    }

    // predefine grid so that errors are not thrown
    WorldImage grid;

    // if number of guesses remaining is 0, then we do not need to draw the current row
    // of guesses because it will result in a grid with one too many rows.
    if (numGuesses <= 0) {
      grid = new AboveImage(
          rowsOfGuesses.foldr(new DrawBelow(), new EmptyImage()), 
          rowsOfDisabledSquares.foldr(new DrawBelow(), new EmptyImage()));
    } else {
      grid = new AboveImage(
          rowsOfGuesses.foldr(new DrawBelow(), new EmptyImage()), 
          rowOfCurrentGuess, 
          rowsOfDisabledSquares.foldr(new DrawBelow(), new EmptyImage()));
    }

    return grid;
  }



  /* Template:
   * Fields:
   * this.dictionary    -- IList<String>
   * this.numGuesses    -- int
   * this.guesses       -- IList<IList<String>>
   * this.secretWord    -- IList<String>
   * this.currentGuess  -- IList<String>
   * this.lastInputtedGuess -- IList<String>
   * 
   * Methods:
   * this.makeScene()         -- WorldScene
   * this.onKeyEvent(String)  -- World
   * this.shouldWorldEnd()    -- boolean
   * this.lastScene(String)   -- WorldScene
   * this.draw()              -- WorldImage
   * 
   * Methods on Fields:
   * this.[IList<String>]         -- SEE ILIST<T> FOR ALL METHODS
   * this.[IList<IList<String>>]  -- SEE ILIST<T> FOR ALL METHODS
   * this.rand.nextInt(int max)   -- returns int
   */
}

// represents a world state for Dordle
class Dordle extends World {
  Wordle wordle1;  
  Wordle wordle2;

  //the constructor for a random game
  Dordle(IList<String> dictionary, int numGuesses) {
    this(dictionary, numGuesses, new Random(), new Random());
  }

  //the constructor for testing with a given random
  Dordle(IList<String> dictionary, int numGuesses, Random rand1, Random rand2) {
    wordle1 = new Wordle(dictionary, numGuesses, new Empty<IList<String>>(), 
        (new WordList()).split(dictionary.get(rand1.nextInt(dictionary.size())), 1, 0), 
        new Empty<String>(), new Empty<String>());
    wordle2 = new Wordle(dictionary, numGuesses, new Empty<IList<String>>(), 
        (new WordList()).split(dictionary.get(rand1.nextInt(dictionary.size())), 1, 0), 
        new Empty<String>(), new Empty<String>());
  }

  // the constructor for testing with a given random
  Dordle(Wordle wordle1, Wordle wordle2) {
    this.wordle1 = wordle1;
    this.wordle2 = wordle2;
  }

  // draws the current world state.
  public WorldScene makeScene() {
    return new WorldScene(400, 400).placeImageXY(wordle1.draw(),100,200)
        .placeImageXY(wordle2.draw(), 300, 200);
  }

  // when the user types in a letter, update the currentGuess
  // list.
  public World onKeyEvent(String key) {
    return new Dordle((Wordle) wordle1.onKeyEvent(key), (Wordle) wordle2.onKeyEvent(key));
  }

  //checks whether we ran out of guesses
  public boolean shouldWorldEnd() {
    return wordle1.shouldWorldEnd() || wordle2.shouldWorldEnd();
  }

  // creates the last scene
  public WorldScene lastScene(String msg) {
    return this.makeScene();
  }

  /* Template:
   * Fields:
   * this.wordle1   -- Wordle
   * this.wordle2   -- Wordle
   * 
   * Methods:
   * this.makeScene()         -- WorldScene
   * this.onKeyEvent(String)  -- World
   * this.shouldWorldEnd() -- boolean
   * this.lastScene(String)   -- WorldScene
   */
}

// list of words:
class WordList {
  String fiveLetters =
      "WHICH,THEIR,WOULD,THERE,COULD,ABOUT,OTHER,THESE,FIRST,AFTER,THINK," +
          "YEARS,THOSE,BEING,THREE,THERE,STILL,WHERE,MIGHT,WORLD,AGAIN,NEVER," +
          "UNDER,WHILE,HOUSE,WHERE,ABOUT,LOCAL,PLACE,GREAT,SMALL,GROUP,QUITE," +
          "PARTY,EVERY,WOMEN,OFTEN,GIVEN,MONEY,POINT,NIGHT,STATE,TAKEN,RIGHT," +
          "THING,WATER,RIGHT,LARGE,ASKED,POWER,LATER,YOUNG,SINCE,TIMES,COURT," +
          "DOING,EARLY,TODAY,USING,WORDS,CHILD,UNTIL,LEVEL,KNOWN,MAJOR,BEGAN," +
          "AREAS,AFTER,WOMAN,AMONG,CLEAR,STAFF,BLACK,WHOLE,SENSE,SEEMS,THIRD," +
          "WHITE,DEATH,SHALL,HEARD,TABLE,WHOSE,ORDER,RANGE,STUDY,TRADE,HOURS," +
          "HANDS,BASED,LEAVE,HUMAN,CASES,CLASS,VOICE,SINCE,SHORT,VALUE,PAPER," +
          "SEVEN,EIGHT,PRICE,RIGHT,UNTIL,MAKES,UNION,TERMS,SOUTH,NORTH,STAGE," +
          "COMES,BRING,WEEKS,START,SHOWN,MUSIC,MONTH,TRIED,WRONG,ISSUE,AWARD," +
          "ROYAL,MOVED,LIGHT,BASIS,FIELD,ADDED,MEANS,ROUND,HEART,ABOVE,STORY," +
          "FORCE,BOARD,STOOD,BOOKS,LEGAL,MODEL,BUILT,FINAL,CLOSE,SPACE,ALONG," +
          "TOTAL,THANK,PRIME,COSTS,TAKES,HAPPY,PARTS,SPENT,FLOOR,ROUND,ALLOW," +
          "RATES,SORRY,HOTEL,MEANT,LOWER,IDEAS,BASIC,WRITE,AWARE,STYLE,RULES," +
          "NEEDS,MAYBE,GIVES,SALES,EVENT,SOUND,READY,LINES,LOOKS,WORTH,PRESS," +
          "BLOOD,GOODS,CARRY,WROTE,GREEN,SHOWS,OFFER,FORMS,MILES,NEEDS,PLANS," +
          "EARTH,TITLE,GIRLS,MEANS,GLASS,HEAVY,SPEAK,RIVER,ABOVE,MOUTH,PIECE," +
          "STAND,EXTRA,WHOLE,OLDER,FULLY,PEACE,WANTS,TYPES,BELOW,RADIO,CIVIL," +
          "FIFTY,START,KNOWS,TREES,LEARN,TRUTH,WORKS,LIVED,SHARE,AGREE,FRONT," +
          "MEDIA,AVOID,STONE,APPLY,LIVES,LATER,CHAIR,HORSE,QUEEN,NAMES,CELLS," +
          "EARLY,VISIT,STOCK,CHIEF,DRAWN,FIRMS,BEGIN,IMAGE,VIEWS,SCALE,PLANT," +
          "SPEND,VOICE,ALONE,TRUST,ENDED,CAUSE,CRIME,UNITS,SPEED,ALONG,SPOKE," +
          "STUFF,FRONT,MATCH,BUILD,REACH,FRESH,SCENE,ITEMS,PHONE,STEPS,WATCH," +
          "FORTY,SIGHT,BANKS,CLAIM,ENJOY,USERS,VIDEO,WORSE,TRAIN,TRIAL,JOINT," +
          "DOUBT,COVER,USUAL,SMILE,SIDES,WHILE,WORKS,AHEAD,RURAL,TWICE,GAMES," +
          "FUNDS,SHAPE,LIGHT,QUIET,POUND,RAISE,OUGHT,NOTED,EQUAL,HOMES,WALLS," +
          "TALKS,OFFER,CAUSE,BREAK,SITES,QUICK,PROVE,NOTES,TRACK,BIRDS,ROUTE," +
          "LIKED,OCCUR,UNDER,ROOMS,DAILY,BELOW,EXIST,CHECK,ALONE,URBAN,YOUTH," +
          "EMPTY,LUNCH,UPPER,SHARE,DRUGS,SERVE,ENTER,WASTE,FACTS,SHOOK,FAITH," +
          "SHOPS,MORAL,HEADS,BIRTH,BROKE,ENTRY,CROWN,VITAL,HOPED,TOTAL,VISIT," +
          "TESTS,OWNER,WIDER,BROAD,DRINK,CLEAN,DOORS,HENCE,TEETH,BRAIN,BRIEF," +
          "SIGNS,COVER,CLAIM,GOALS,GUIDE,DRIVE,IDEAL,BOUND,KINDS,WORRY,MINOR," +
          "SEATS,NOISE,THICK,LOVED,METAL,GRAND,PHASE,COAST,LYING,WORST,ADULT," +
          "FACED,INDEX,SPORT,JUDGE,BROWN,FUNNY,INNER,LEAST,PAGES,SHARP,DRIVE," +
          "NAMED,SIXTY,AGENT,BADLY,PLACE,CROSS,GROWN,CROWD,ARGUE,CATCH,TEARS," +
          "ALIVE,BEGUN,YOURS,ANGRY,SHEET,MOTOR,SHOCK,CLOSE,DRESS,GRASS,FRUIT," +
          "TOWNS,LUCKY,TOUCH,PLATE,TIRED,FIGHT,SLEEP,TEAMS,STARS,CHEAP,CARDS," +
          "ROADS,GRANT,THEME,ERROR,DREAM,HELLO,CHEST,REFER,BEACH,FOCUS,CLUBS," +
          "BREAD,ADMIT,CHIEF,STEEL,LEADS,WAGES,TASKS,PANEL,YARDS,CHAIN,TELLS," +
          "ARMED,SLEEP,SHOES,DROVE,FALSE,SUGAR,BLOCK,ASIDE,STORE,BREAK,RAPID," +
          "FIXED,AIMED,OWNED,DRAMA,UNCLE,FIFTH,LINKS,SOLID,APART,SKILL,CLOSE," +
          "DEALT,FILMS,ROUND,TASTE,PLANE,SCOPE,FAULT,ENEMY,ROUGH,LIMIT,ABUSE," +
          "TOWER,ANGER,SWEET,ARISE,POINT,FACES,FEELS,COSTS,INPUT,TOUGH,SAVED," +
          "TRULY,DRINK,TURNS,TOOLS,CYCLE,NURSE,FRAME,PROUD,PILOT,CALLS,GIVEN," +
          "VOTES,CREAM,FEWER,THROW,AWFUL,THREW,HILLS,PRIZE,NOVEL,DEPTH,CALLS," +
          "BILLS,REPLY,TREAT,GREEN,SHEEP,STUDY,CRIED,SOUND,DANCE,SORTS,PRESS," +
          "FILES,FIGHT,STILL,ROCKS,PLAIN,WATCH,SINCE,FINDS,RATIO,COACH,FEARS," +
          "SMOKE,RUGBY,SONGS,CLOCK,FIXED,HELPS,CHOSE,MINDS,MARKS,TRUST,STEAM," +
          "SILLY,TEACH,UNITY,TAXES,SHIRT,ROUND,FINAL,BEING,ROLES,SCORE,LOANS," +
          "DOZEN,PRIDE,NEWLY,BUYER,MATCH,SHIPS,WAVES,KNIFE,PROOF,MARRY,LIVES," +
          "PITCH,BOXES,HOLES,ABOVE,APPLE,DIRTY,HOLDS,TREND,LOOSE,STATE,BLIND," +
          "KNEES,BOOTS,SMELL,MUMMY,KEEPS,WHEEL,TOUCH,SHIFT,DRAFT,SQUAD,FLESH," +
          "SPLIT,SIXTH,LEVEL,ROOTS,STICK,LAYER,RISKS,CURVE,ADOPT,GUARD,OUTER," +
          "TOPIC,TENDS,HOPES,ANGLE,GUESS,WINGS,CLEAR,MEALS,RULED,PLAYS,TEXTS," +
          "TIGHT,OPERA,PUPIL,BLAME,MIXED,GUEST,LOGIC,ACUTE,VOTED,ACTED,DELAY," +
          "VALID,HABIT,BONES,CROSS,URGED,STORM,GROSS,STUCK,MALES,PAINT,POSTS," +
          "EXACT,DADDY,AUDIT,LISTS,ALBUM,LEAVE,FORCE,FALLS,CANAL,FOCUS,MAYOR," +
          "WORTH,COUNT,DOUBT,CRASH,PRINT,LAUGH,PAIRS,LEASE,GAINS,FOODS,ALARM," +
          "SHEER,COUNT,CLOUD,GENES,LIKES,LANDS,SWEPT,NAKED,ASSET,BANDS,ACTOR," +
          "CRAFT,PLANS,DIARY,OCEAN,BENCH,MIXED,BOATS,JUDGE,KNOWN,BIBLE,MOVES," +
          "FIRED,CLOTH,SHELL,PIANO,CLERK,GATES,BONDS,WIVES,SOLVE,SADLY,SPARE," +
          "GRADE,STAKE,ASIAN,CHEEK,ALTER,SHAME,DATES,ABBEY,FLEET,STAND,FLATS," +
          "DEBTS,BURST,STICK,FAILS,LOCAL,CABLE,CHECK,CHIPS,ORDER,BRICK,GIANT," +
          "HOPES,FARMS,GRAIN,FRAUD,SWUNG,NASTY,MOVIE,SHOWS,FORUM,RELAX,CRAZY," +
          "SHOTS,REIGN,GUILT,LOVER,SLEPT,UPSET,FORMS,MOUSE,SIZES,VILLA,EDGES," +
          "PANIC,LABEL,THEFT,RISEN,DEVIL,GIFTS,DYING,MAGIC,BRAVE,LAUGH,OPENS," +
          "EATEN,GLORY,FENCE,JUICE,HATED,LIVER,SEEDS,MOVES,CHAOS,RANKS,ISSUE," +
          "CLEAN,TRAIN,POEMS,DRUNK,PAUSE,STRIP,SUPER,ACRES,ESSAY,AROSE,PATCH," +
          "CROPS,LIMIT,RACES,CLIMB,WIDOW,STEEP,DEBUT,CHART,WOODS,GRACE,BASES," +
          "HARSH,LORDS,FIBRE,BRASS,BALLS,FAINT,ROSES,FLUID,SEEKS,VAGUE,VIRUS," +
          "SHIFT,NAVAL,SHOOT,KINGS,WAVED,ADDED,MAGIC,SWORD,IMPLY,BLANK,SMART," +
          "TANKS,TRIES,BUSES,SHORE,LOADS,STIFF,CITED,RIGID,TRICK,MINES,DRANK," +
          "TAPES,EAGER,SKIRT,GRIEF,PARKS,PHONE,SHELF,WAIST,WASTE,SAUCE,COINS," +
          "DANCE,WINDS,MEETS,BORED,BONUS,DAILY,CRUEL,FACES,VERSE,GHOST,SHADE," +
          "FATAL,SLOPE,ANGEL,STRAW,UPSET,RIVAL,LOYAL,PATHS,SCORE,NOBLE,NAILS," +
          "LORRY,BRAND,LOOKS,ORGAN,CARED,MANOR,CRUDE,BEANS,BRUSH,SPELL,DATED," +
          "NERVE,PENCE,SERUM,AWAKE,BLOKE,FORTH,MINUS,RIDGE,POSED,PAINT,GROWS," +
          "SUITE,REACH,OZONE,REACT,DEALS,JEANS,TALES,RALLY,GRANT,STUCK,EAGLE," +
          "CHARM,GRAVE,CODES,REPLY,HUMAN,SOLAR,POLES,SHAKE,BLACK,BOWEL,PHOTO," +
          "SPOTS,KNOCK,BLUES,SOUND,LOVES,PRIOR,BREED,GUIDE,MODES,BUNCH,FIRES," +
          "STOPS,TOXIC,LEMON,BASIN,RINGS,SWING,FLOOD,TRAIL,LAKES,FETCH,BOMBS," +
          "LINED,PENNY,WALKS,VENUE,DEALS,BLOWN,TILES,FANCY,CRACK,HEELS,TRUCK," +
          "PLAYS,ALIKE,SMELL,WIPED,TRACE,USAGE,CORPS,ZONES,BACKS,PIPES,WIDTH," +
          "WHITE,SMOKE,CAMPS,GAZED,SALAD,ARRAY,MAJOR,PLAIN,TENTH,SKULL,JOKES," +
          "POOLS,TWINS,BORNE,YIELD,THUMB,DYING,CLASH,ARMED,WOUND,CABIN,MEDAL," +
          "TRIPS,MERCY,BLADE,DRAWS,STAMP,FERRY,ALPHA,FLOWN,ELBOW,CLIFF,NOVEL," +
          "SWEAT,PAINS,HONEY,WEIRD,TUTOR,PORTS,FLUNG,FEVER,TIGHT,WINES,SMILE," +
          "FINED,MARCH,POLLS,LIMBS,MOUNT,TRACE,PULSE,WRIST,ATOMS,BRIDE,REALM," +
          "CREWS,FLAME,FLOUR,PRINT,BOOST,LASER,YACHT,ARROW,VIVID,NOISY,QUOTE," +
          "GRAPH,BOOST,BURNT,CEASE,SHOUT,CHOIR,ACIDS,MAKER,TOURS,SPARE,ADAPT," +
          "CIVIC,BELLS,ALTOS,STEAL";

  String fourLetterWords =
      "ABLE,ACID,AGED,ALSO,AREA,ARMY,AWAY,BABY,BACK,BALL,BAND,BANK,BASE," +
          "BATH,BEAR,BEAT,BEEN,BEER,BELL,BELT,BEST,BILL,BIRD,BLOW,BLUE,BOAT," +
          "BODY,BOMB,BOND,BONE,BOOK,BOOM,BORN,BOSS,BOTH,BOWL,BULK,BURN,BUSH," +
          "BUSY,CALL,CALM,CAME,CAMP,CARD,CARE,CASE,CASH,CAST,CELL,CHAT,CHIP," +
          "CITY,CLUB,COAL,COAT,CODE,COLD,COME,COOK,COOL,COPE,COPY,CORE,COST," +
          "CREW,CROP,DARK,DATA,DATE,DAWN,DAYS,DEAD,DEAL,DEAN,DEAR,DEBT,DEEP," +
          "DENY,DESK,DIAL,DIET,DIRT,DISC,DISK,DOES,DONE,DOOR,DOSE,DOWN,DRAW," +
          "DREW,DROP,DRUG,DUAL,DUKE,DUST,DUTY,EACH,EARN,EASE,EAST,EASY,EDGE," +
          "ELSE,EVEN,EVER,EVIL,EXIT,FACE,FACT,FAIL,FAIR,FALL,FARM,FAST,FATE," +
          "FEAR,FEED,FEEL,FEET,FELL,FELT,FILE,FILL,FILM,FIND,FINE,FIRE,FIRM," +
          "FISH,FIVE,FLAT,FLOW,FOOD,FOOT,FORD,FORM,FORT,FOUR,FREE,FROM,FUEL," +
          "FULL,FUND,GAIN,GAME,GATE,GAVE,GEAR,GENE,GIFT,GIRL,GIVE,GLAD,GOAL," +
          "GOES,GOLD,GOLF,GONE,GOOD,GRAY,GREW,GREY,GROW,GULF,HAIR,HALF,HALL," +
          "HAND,HANG,HARD,HARM,HATE,HAVE,HEAD,HEAR,HEAT,HELD,HELL,HELP,HERE," +
          "HERO,HIGH,HILL,HIRE,HOLD,HOLE,HOLY,HOME,HOPE,HOST,HOUR,HUGE,HUNG," +
          "HUNT,HURT,IDEA,INCH,INTO,IRON,ITEM,JACK,JANE,JEAN,JOHN,JOIN,JUMP," +
          "JURY,JUST,KEEN,KEEP,KENT,KEPT,KICK,KILL,KIND,KING,KNEE,KNEW,KNOW," +
          "LACK,LADY,LAID,LAKE,LAND,LANE,LAST,LATE,LEAD,LEFT,LESS,LIFE,LIFT," +
          "LIKE,LINE,LINK,LIST,LIVE,LOAD,LOAN,LOCK,LOGO,LONG,LOOK,LORD,LOSE," +
          "LOSS,LOST,LOVE,LUCK,MADE,MAIL,MAIN,MAKE,MALE,MANY,MARK,MASS,MATT," +
          "MEAL,MEAN,MEAT,MEET,MENU,MERE,MIKE,MILE,MILK,MILL,MIND,MINE,MISS," +
          "MODE,MOOD,MOON,MORE,MOST,MOVE,MUCH,MUST,NAME,NAVY,NEAR,NECK,NEED," +
          "NEWS,NEXT,NICE,NICK,NINE,NONE,NOSE,NOTE,OKAY,ONCE,ONLY,ONTO,OPEN," +
          "ORAL,OVER,PACE,PACK,PAGE,PAID,PAIN,PAIR,PALM,PARK,PART,PASS,PAST," +
          "PATH,PEAK,PICK,PINK,PIPE,PLAN,PLAY,PLOT,PLUG,PLUS,POLL,POOL,POOR," +
          "PORT,POST,PULL,PURE,PUSH,RACE,RAIL,RAIN,RANK,RARE,RATE,READ,REAL," +
          "REAR,RELY,RENT,REST,RICE,RICH,RIDE,RING,RISE,RISK,ROAD,ROCK,ROLE," +
          "ROLL,ROOF,ROOM,ROOT,ROSE,RULE,RUSH,RUTH,SAFE,SAID,SAKE,SALE,SALT," +
          "SAME,SAND,SAVE,SEAT,SEED,SEEK,SEEM,SEEN,SELF,SELL,SEND,SENT,SEPT," +
          "SHIP,SHOP,SHOT,SHOW,SHUT,SICK,SIDE,SIGN,SITE,SIZE,SKIN,SLIP,SLOW," +
          "SNOW,SOFT,SOIL,SOLD,SOLE,SOME,SONG,SOON,SORT,SOUL,SPOT,STAR,STAY," +
          "STEP,STOP,SUCH,SUIT,SURE,TAKE,TALE,TALK,TALL,TANK,TAPE,TASK,TEAM," +
          "TECH,TELL,TEND,TERM,TEST,TEXT,THAN,THAT,THEM,THEN,THEY,THIN,THIS," +
          "THUS,TILL,TIME,TINY,TOLD,TOLL,TONE,TONY,TOOK,TOOL,TOUR,TOWN,TREE," +
          "TRIP,TRUE,TUNE,TURN,TWIN,TYPE,UNIT,UPON,USED,USER,VARY,VAST,VERY," +
          "VICE,VIEW,VOTE,WAGE,WAIT,WAKE,WALK,WALL,WANT,WARD,WARM,WASH,WAVE," +
          "WAYS,WEAK,WEAR,WEEK,WELL,WENT,WERE,WEST,WHAT,WHEN,WHOM,WIDE,WIFE," +
          "WILD,WILL,WIND,WINE,WING,WIRE,WISE,WISH,WITH,WOOD,WORD,WORE,WORK," +
          "YARD,YEAH,YEAR,YOUR,ZERO,ZONE";


  // Splits a given string into chunks of a given positive length,
  // that are separated by gaps of a given non-negative length.
  IList<String> split(String string, int chunkLength, int sepLength) {
    if (string.length() <= chunkLength) {
      // if the string is less than chunk length
      return new Cons<String>(string, new Empty<String>());
    } else if (string.length() <= chunkLength + sepLength) {
      // if its more than chunk length but nothing comes after the separation
      return new Cons<String>(string.substring(0, chunkLength), new Empty<String>()); 
    } else {
      // if there is more we can split
      return new Cons<String>(string.substring(0, chunkLength),
          split(string.substring(chunkLength + sepLength), chunkLength, sepLength));
    }
  }

  /* Template:
   * Fields:
   * this.fiveLetterWords   -- String
   * this.fourLetterWords   -- String
   * 
   * Methods:
   * this.split(String, int, int)   -- IList<String>
   */
}

// represents a pair of two Objects
class Pair<L, R> {
  L left;
  R right;

  // the constructor
  Pair(L left, R right) { 
    this.left = left; 
    this.right = right; 
  }

  /* Template:
   * Fields:
   * this.left    -- L
   * this.right   -- R
   */
}

// represents a yes/no question about T.
interface IPred<T> {
  // applies the predicate to the T
  boolean apply(T item);

  /* Template:
   * Methods:
   * apply(T)   -- boolean
   */
}

// represents a unary function
interface IFunc<A, R> {
  // applies the unary function on type A and returns
  // type R.
  R apply(A arg);

  /* Template:
   * Methods:
   * this.apply(A)   -- R
   */
}

// represents a two-ary function
interface IFunc2<A1,A2, R> {
  // applies the two-ary function on A1 and A2,
  // returning R
  R apply(A1 arg1, A2 arg2);

  /* Template:
   * Methods:
   * this.apply(A1, A2)    -- R
   */
}

// represents a three-ary function
interface IFunc3<A1, A2, A3, R> {
  // applies the three-ary function on A1, A2, A3,
  // returning R
  R apply(A1 arg1, A2 arg2, A3 arg3);

  /* Template:
   * Methods:
   * this.apply(A1, A2, A3)   -- R
   */
}

// represents a function objects for general draw related functions
class Util {
  // universal constants
  int gridSize = 30; 
  WorldImage buffer = new RectangleImage(2, 2, OutlineMode.SOLID, Color.WHITE);
  // the wordLength of the Wordle that the Util is called for.
  int wordLength;

  // the default constructor
  Util() {
    this.wordLength = 5;
  }

  // the constructor
  Util(int wordLength) {
    this.wordLength = wordLength;
  }

  // draws a blank square
  public WorldImage drawSquare() {
    return this.drawSquare("", Color.GRAY);
  }

  // draws a square using the specified String letter and Color.
  public WorldImage drawSquare(String letter, Color c) {
    return new OverlayImage(new TextImage(letter, gridSize, Color.WHITE), 
        new OverlayImage(new RectangleImage(gridSize, gridSize, OutlineMode.OUTLINE, Color.GRAY),
            new RectangleImage(gridSize, gridSize, OutlineMode.SOLID, c)));
  }

  // Appends one block to another block (with a buffer in the middle)
  public WorldImage addBlock(WorldImage block, WorldImage rest) {
    return new BesideImage(block, buffer, rest);
  }

  // adds the specified number of "val"s to the end of the current IList<String> 
  public IList<String> addLettersToEnd(IList<String> cur, String val, int add) {
    if (add == 0) {
      return cur;
    } else {
      return this.addLettersToEnd(cur.pushBack(val), val, add - 1);
    }
  }

  // draws a row of disabled squares.
  public WorldImage drawRowOfDisabledSquares() {
    IList<String> rowLetters = new Cons<String>("", wordLength);
    IList<Color> colors = new Cons<Color>(Color.GRAY, wordLength);
    IList<Pair<Color, String>> colorStringPairs = colors.map2(rowLetters, new Zipper<>());
    return colorStringPairs.foldr(new DrawLeft(), new EmptyImage());
  }

  // draws a row that represents the current guess IList<String>
  // (in order to do this, it must first complete the row by filling in the
  // rest of the letters with "").
  public WorldImage completeRowCurrentGuess(IList<String> currentGuess) {
    Util util = new Util();
    IList<String> rowLetters = currentGuess;
    int currentSize = rowLetters.size();
    IList<Boolean> greensBool = new Cons<Boolean>(false, wordLength);
    IList<Boolean> yellowsBool = new Cons<Boolean>(false, wordLength);
    IList<Color> colors = greensBool.map2(yellowsBool, new GetCorrectColor());
    IList<Pair<Color, String>> colorStringPairs = 
        colors.map2(util.addLettersToEnd(rowLetters, "", wordLength - currentSize), 
            new Zipper<>());
    return colorStringPairs.foldr(new DrawLeft(), new EmptyImage());
  }

  // draws the row given a row of letters and the secret word.
  public WorldImage drawRow(IList<String> rowLetters, IList<String> secretWord) {
    IList<Boolean> greensBool = rowLetters.samePositionAndValue(secretWord, new StringEquality());
    IList<String> secretWordReplaced = secretWord.map2(greensBool, new ReplaceWhenTrue());
    IList<Boolean> yellowsBool = rowLetters
        .containsWithReplacement(secretWordReplaced, "_", new StringEquality());
    IList<Color> colors = greensBool.map2(yellowsBool, new GetCorrectColor());
    IList<Pair<Color, String>> colorStringPairs = colors.map2(rowLetters, new Zipper<>());
    return colorStringPairs.foldr(new DrawLeft(), new EmptyImage());
  }

  /* Template:
   * Fields:
   * this.gridSize    -- int
   * this.buffer      -- WorldImage
   * this.wordLength  -- int
   * 
   * Methods:
   * this.drawSquare()    -- WorldImage
   * this.drawSquare(String, Color)   -- WorldImage
   * this.addBlock(WorldImage, WorldImage)    -- WorldImage
   * this.addLettersToEnd(IList<String>,String, int)    -- IList<String>
   * this.drawRowOfDisabledSquares()    -- WorldImage
   * this.completeRowCurrentGuess(IList<String>)    -- WorldImage
   * this.drawRow(IList<String>, IList<String>)   -- WorldImage
   */
}

// is a function object that represents appending one string to the end of another.
class StringAppend implements IFunc2<String, String, String> {
  // appends one string to the end of another. 
  public String apply(String s1, String s2) {
    return s1 + s2;
  }

  /* Template:
   * Methods:
   * apply(String, String)    -- String
   * 
   */
}

// is a function object that converts a list of letters to its corresponding
// row in the grid
class DrawRow implements IFunc<IList<String>, WorldImage> {
  IList<String> secretWord;

  // the constructor requires the secret word
  DrawRow(IList<String> secretWord) {
    this.secretWord = secretWord;
  }

  // creates the row corresponding to the given row of letters.
  public WorldImage apply(IList<String> rowLetters) {
    Util util = new Util();
    return util.drawRow(rowLetters, this.secretWord);
  }

  /* Template:
   * Methods:
   * this.apply(IList<String>)    -- WorldImage
   */
}

// is a function object that takes two Booleans and returns a
// color
class GetCorrectColor implements IFunc2<Boolean, Boolean, Color> {
  // applies the function that parses two Booleans and returns
  // the corresponding color on the Wordle grid.
  public Color apply(Boolean b1, Boolean b2) {
    if (b1) {
      return Color.GREEN;
    } else if (b2) {
      return new Color(230, 230, 0);
    } else {
      return Color.GRAY;
    }
  }

  /* Template:
   * Methods:
   * this.apply(Boolean, Boolean)   -- Color
   */
}

// a function object that draws one image below another image
class DrawBelow implements IFunc2<WorldImage, WorldImage, WorldImage> {
  // draws one image below another image.
  public WorldImage apply(WorldImage below, WorldImage above) {
    return new AboveImage(above, below);
  }

  /* Template:
   * Methods:
   * this.apply(WorldImage, WorldImage)   -- WorldImage
   */
}

// appends a box to the left of the rest of the WorldImage
// (in a foldr, this function object will return a row)
class DrawLeft implements IFunc2<Pair<Color, String>, WorldImage, WorldImage> {
  // applies the function object and returns a larger WorldImage.
  public WorldImage apply(Pair<Color, String> colorAndString, 
      WorldImage restOfImage) {
    Util util = new Util();
    return util.addBlock(util.drawSquare(colorAndString.right, colorAndString.left), restOfImage);
  }



  /* Template:
   * Methods:
   * this.apply(Pair<Color, String>, WorldImage)    -- WorldImage
   */
}

// function object to zip two args into a pair
class Zipper<T1, T2> implements IFunc2<T1, T2, Pair<T1, T2>> {
  // zips the two arguments together and returns a pair.
  public Pair<T1, T2> apply(T1 arg1, T2 arg2) {
    return new Pair<T1, T2>(arg1, arg2);
  }

  /* Template:
   * Methods:
   * this.apply(T1, T2)   -- Pair<T1, T2>
   */
}

// checks if the first String is equal to the second String.
class StringEquality implements IFunc2<String, String, Boolean> {
  // is the first String the same as the second String?
  public Boolean apply(String s1, String s2) {
    return s1.equals(s2);
  }

  /* Template:
   * Methods:
   * this.apply(String, String)   -- Boolean
   */
}

// represents a function object that replaces the string
// with the replacement given in the constructor when the 
// Boolean is true
class ReplaceWhenTrue implements IFunc2<String, Boolean, String> {
  String replacement;

  // the default constructor
  ReplaceWhenTrue() {
    this.replacement = "_";
  }

  // the constructor
  ReplaceWhenTrue(String replacement) {
    this.replacement = replacement;
  }

  // replaces the string with the replacement when the Boolean is true.
  public String apply(String s, Boolean b) {
    if (b) {
      return replacement;
    } else {
      return s;
    }
  }

  /* Template:
   * Fields:
   * this.replacement     -- String
   * 
   * Methods:
   * this.apply(String, Boolean)    -- String
   */
}


interface IList<T> {
  // puts the element at the end of the list
  IList<T> pushBack(T element);

  // returns a new list with the last element removed
  IList<T> removeLast();

  // returns a new list with the last element removed
  // Accumulator: prev is the prev element, so that 
  // when we hit the very end we know of
  // and delete this element by not including it
  IList<T> removeLastHelper(T prev);


  // reverses the IList
  IList<T> reverse();

  // reverses the IList
  // Accumulator: accumulates the prev elements in reversed order
  IList<T> reverseHelper(IList<T> prev);

  // returns the T at the given index.
  T get(int index);

  // returns the number of T's in the list
  int size();

  // checks if all of the list follows the predicate
  boolean sameList(IList<T> other, IFunc2<T, T, Boolean> equalityPred);

  // checks if all T's in this and the Cons are equal according to the equality pred.
  boolean sameCons(Cons<T> other, IFunc2<T, T, Boolean> equalityPred);

  // checks if all T's in the this and the Empty are equal according to the equality pred.
  boolean sameEmpty(Empty<T> other, IFunc2<T, T, Boolean> equalityPred);

  // returns a list of Booleans such that when the
  // position and the value is the same, the Boolean at
  // that position is a true, otherwise, if the position or the
  // value is different between this list and other list, 
  // the value at that position is false.
  IList<Boolean> samePositionAndValue(IList<T> other, IFunc2<T, T, Boolean> equalityPred);

  // for each element in this list, it will check if it is contained in the other list,
  // and ensures that the number of times that the element is contained in this list does not
  // exceed the number of times it is contained in the other list.
  IList<Boolean> containsWithReplacement(IList<T> other, T replacement, 
      IFunc2<T, T, Boolean> equalityPred);

  // returns a list of T with the replaces 
  // the first instance of the given T with the replacement T
  IList<T> replaceFirstOccurrence(T target, T replacement, IFunc2<T, T, Boolean> equalityPred);

  // folds the list from the right.
  <R> R foldr(IFunc2<T, R, R> func, R base);

  // maps the list using the given IFunc
  <R> IList<R> map(IFunc<T, R> convert);

  // maps two ILists into a single IList<R>
  <R, T2> IList<R> map2(IList<T2> other, IFunc2<T, T2, R> functor);

  // maps one IList and a Cons into a single IList<R>
  <R, T2> IList<R> map2Cons(Cons<T2> other, IFunc2<T2, T, R> functor);

  // maps one IList and an Empty into a single IList<R>
  <R, T2> IList<R> map2Mt(Empty<T2> other, IFunc2<T2, T, R> functor);

  // does this list contain the target?
  boolean contains(T target, IFunc2<T, T, Boolean> equalityPred);

  /* Template:
   * Methods:
   * this.pushBack(T)                                           -- IList<T>
   * this.removeLast()                                          -- IList<T>
   * this.removeLastHelper(T)                                   -- IList<T>
   * this.reverse()                                             -- IList<T>
   * this.reverseHelper(IList<T>)                               -- IList<T>
   * this.get(int)                                              -- T
   * this.size()                                                -- int
   * this.sameList(IList<T>, IFunc2<T, T, Boolean>)             -- Boolean
   * this.sameCons(Cons<T>, IFunc2<T, T, Boolean>)              -- Boolean
   * this.sameEmpty(Empty<T>, IFunc2<T, T, Boolean>)            -- Boolean
   * this.samePositionAndValue(IList<T>, IFunc<T, T, Boolean>)  -- IList<Boolean>
   * this.containsWithReplacement(IList<T>, T, IFunc2<T, T, Boolean>)    -- IList<Boolean>
   * this.replaceFirstOccurence(T, T, IFunc2<T, T, Boolean>)    -- IList<T>
   * this.foldr(IFunc2<T, R, R>, R)                             -- R
   * this.map(IFunc<T, R>)                                      -- R
   * this.map2(IList<T2>, IFunc2<T, T2, R>)                     -- IList<R>
   * this.map2Cons(Cons<T2>, IFunc2<T, T2, R>)                  -- IList<R>
   * this.map2Mt(Empty<T2>, IFunc2<T, T2, R>)                   -- IList<R>
   * this.contains(T, IFunc2<T, T, Boolean>)                    -- Boolean
   */
}

// represents an empty list of T
class Empty<T> implements IList<T> {
  // puts the element at the end of the list
  public IList<T> pushBack(T element) {
    return new Cons<T>(element, this);
  }

  // returns a new list with the last element removed
  public IList<T> removeLast() {
    return this;
  }

  // returns a new list with the last element removed
  // Accumulator: prev is the prev element, so that 
  // when we hit the very end we know of
  // and delete this element by not including it
  public IList<T> removeLastHelper(T prev) {
    return this;
  }

  // reverses the Empty
  public IList<T> reverse() {
    return this;
  }

  // reverses the Empty
  // Accumulator: accumulates the prev elements in reversed order
  public IList<T> reverseHelper(IList<T> prev) {
    return prev;
  }

  // maps this list given the convert function object
  public <R> IList<R> map(IFunc<T, R> convert) {
    return new Empty<R>();
  }

  // maps this and an IList into a single IList<R>
  public <R, T2> IList<R> map2(IList<T2> other, IFunc2<T, T2, R> functor) {
    return other.map2Mt(this, functor);
  }

  // maps this and a Cons into a single IList<R>
  public <R, T2> IList<R> map2Cons(Cons<T2> other, IFunc2<T2, T, R> functor) {
    throw new RuntimeException("List arguments are different lengths");
  }

  // maps this and an Empty into a single IList<R>
  public <R, T2> IList<R> map2Mt(Empty<T2> other, IFunc2<T2, T, R> functor) {
    return new Empty<R>();
  }

  //returns the T at the given index.
  public T get(int index) {
    throw new RuntimeException("List index out of bounds");
  }

  // returns the number of T's in the Empty
  public int size() {
    return 0;
  }

  // is this the same as another IList?
  public boolean sameList(IList<T> other, IFunc2<T, T, Boolean> equalityPred) {
    return other.sameEmpty(this, equalityPred);
  }

  // is this the same as a Cons?
  public boolean sameCons(Cons<T> other, IFunc2<T, T, Boolean> equalityPred) {
    return false;
  }

  // is this the same as an Empty?
  public boolean sameEmpty(Empty<T> other, IFunc2<T, T, Boolean> equalityPred) {
    return true;
  }

  // returns a list of T with the replaces 
  // the first instance of the given T with the replacement T
  public IList<T> replaceFirstOccurence(T target, T replacement) {
    return this;
  }

  // returns a list of Booleans such that when the
  // position and the value is the same, the Boolean at
  // that position is a true, otherwise, if the position or the
  // value is different between this list and other list, 
  // the value at that position is false.
  public IList<Boolean> samePositionAndValue(IList<T> other, IFunc2<T, T, Boolean> equalityPred) {
    return map2(other, equalityPred);
  }

  // for each element in this list, it will check if it is contained in the other list,
  // and ensures that the number of times that the element is contained in this list does not
  // exceed the number of times it is contained in the other list.
  public IList<Boolean> containsWithReplacement(IList<T> other, T replacement,
      IFunc2<T, T, Boolean> equalityPred) {
    return new Empty<Boolean>();
  }

  // returns a list of T that replaces the first instance of the given T with the replacement T
  public IList<T> replaceFirstOccurrence(T target, T replacement,
      IFunc2<T, T, Boolean> equalityPred) {
    return new Empty<T>();
  }

  // folds the list from the right.
  public <R> R foldr(IFunc2<T, R, R> func, R base) {
    return base;
  }

  // does this Empty contain the target?
  public boolean contains(T target, IFunc2<T, T, Boolean> equalityPred) {
    return false;
  }


  /* Template:
   * Methods:
   * this.pushBack(T)                                           -- IList<T>
   * this.removeLast()                                          -- IList<T>
   * this.removeLastHelper(T)                                   -- IList<T>
   * this.reverse()                                             -- IList<T>
   * this.reverseHelper(IList<T>)                               -- IList<T>
   * this.get(int)                                              -- T
   * this.size()                                                -- int
   * this.sameList(IList<T>, IFunc2<T, T, Boolean>)             -- Boolean
   * this.sameCons(Cons<T>, IFunc2<T, T, Boolean>)              -- Boolean
   * this.sameEmpty(Empty<T>, IFunc2<T, T, Boolean>)            -- Boolean
   * this.samePositionAndValue(IList<T>, IFunc<T, T, Boolean>)  -- IList<Boolean>
   * this.containsWithReplacement(IList<T>, T, IFunc2<T, T, Boolean>)    -- IList<Boolean>
   * this.replaceFirstOccurence(T, T, IFunc2<T, T, Boolean>)    -- IList<T>
   * this.foldr(IFunc2<T, R, R>, R)                             -- R
   * this.map(IFunc<T, R>)                                      -- R
   * this.map2(IList<T2>, IFunc2<T, T2, R>)                     -- IList<R>
   * this.map2Cons(Cons<T2>, IFunc2<T, T2, R>)                  -- IList<R>
   * this.map2Mt(Empty<T2>, IFunc2<T, T2, R>)                   -- IList<R>
   * this.contains(T, IFunc2<T, T, Boolean>)                    -- Boolean
   */
}

// represents a non-empty list of T.
class Cons<T> implements IList<T> {
  T first;
  IList<T> rest;

  // the constructor
  Cons(T first, IList<T> rest) {
    this.first = first;
    this.rest = rest;
  }

  //the constructor
  Cons(T val, int size) {
    if (size == 1) {
      this.first = val;
      this.rest = new Empty<T>();
    } else {
      this.first = val;
      this.rest = new Cons<T>(val, size - 1);
    }
  }

  // puts the element at the end of the list
  public IList<T> pushBack(T element) {
    return new Cons<T>(this.first, this.rest.pushBack(element));
  }

  // returns a new list with the last element removed
  public IList<T> removeLast() {
    return this.rest.removeLastHelper(this.first);
  }

  // returns a new list with the last element removed
  // Accumulator: prev is the prev element, so that 
  // when we hit the very end we know of
  // and delete this element by not including it
  public IList<T> removeLastHelper(T prev) {
    return new Cons<T>(prev, this.rest.removeLastHelper(this.first));
  }

  // reverses the Cons
  public IList<T> reverse() {
    return reverseHelper(new Empty<T>());
  }

  // reverses the Cons
  // Accumulator: accumulates the prev elements in reversed order
  public IList<T> reverseHelper(IList<T> prev) {
    // Augmented template:
    // prev has template of IList<T>, so see IList<T>'s template
    return this.rest.reverseHelper(new Cons<T>(this.first, prev));
  }

  // maps this list given the convert function object
  public <R> IList<R> map(IFunc<T, R> convert) {
    return new Cons<R>(convert.apply(this.first), this.rest.map(convert));
  }

  // maps this and another List into a single IList<R>
  public <R, T2> IList<R> map2(IList<T2> other, IFunc2<T, T2, R> functor) {
    // Augmented template:
    // other has template of IList<T2>, so see IList<T>'s template
    // functor.apply(T, T2) -- R
    return other.map2Cons(this, functor);
  }

  // maps this and another Cons into a single IList<R>
  public <R, T2> IList<R> map2Cons(Cons<T2> other, IFunc2<T2, T, R> functor) {
    // Augmented template:
    // other has template of Cons<T2>, so see Cons<T2>'s template
    // functor.apply(T2, T) -- R
    return new Cons<R>(functor.apply(other.first, this.first), 
        other.rest.map2(this.rest, functor));
    // notice the order, to preserve the fact that the first list (which is now "other") 
    // should come first
  }

  // maps this and other Empty into a single IList<R>
  public <R, T2> IList<R> map2Mt(Empty<T2> other, IFunc2<T2, T, R> functor) {
    throw new RuntimeException("List arguments are different lengths");
  }

  // gets the T at the specified index.
  public T get(int index) {
    if (index == 0) {
      return this.first;
    } else {
      return this.rest.get(index - 1);
    }
  }

  // returns the number of T's in the cons
  public int size() {
    return this.rest.size() + 1;
  }

  // is this the same as the other list?
  public boolean sameList(IList<T> other, IFunc2<T, T, Boolean> equalityPred) {
    return other.sameCons(this, equalityPred);
  }

  // is this the same as the other Cons list?
  public boolean sameCons(Cons<T> other, IFunc2<T, T, Boolean> equalityPred) {
    return equalityPred.apply(this.first, other.first) 
        && this.rest.sameList(other.rest, equalityPred);
  }

  // is this the same as the other Empty list?
  public boolean sameEmpty(Empty<T> other, IFunc2<T, T, Boolean> equalityPred) {
    return false;
  }

  // returns a list of Booleans such that when the
  // position and the value is the same, the Boolean at
  // that position is a true, otherwise, if the position or the
  // value is different between this list and other list, 
  // the value at that position is false.
  public IList<Boolean> samePositionAndValue(IList<T> other, IFunc2<T, T, Boolean> equalityPred) {
    return map2(other, equalityPred);
  }

  // for each element in this list, it will check if it is contained in the other list,
  // and ensures that the number of times that the element is contained in this list does not
  // exceed the number of times it is contained in the other list.
  public IList<Boolean> containsWithReplacement(IList<T> other, T replacement,
      IFunc2<T, T, Boolean> equalityPred) {
    if (other.contains(this.first, equalityPred)) {
      IList<T> newOther = other.replaceFirstOccurrence(this.first, replacement, equalityPred);
      return new Cons<Boolean>(true, 
          this.rest.containsWithReplacement(newOther, replacement, equalityPred));
    } else {
      return new Cons<Boolean>(false, 
          this.rest.containsWithReplacement(other, replacement, equalityPred));
    }
  }

  // returns a list of T that replaces the first instance of the given T with the replacement T
  public IList<T> replaceFirstOccurrence(T target, T replacement,
      IFunc2<T, T, Boolean> equalityPred) {
    if (equalityPred.apply(this.first, target)) {
      return new Cons<T>(replacement, this.rest);
    } else {
      return new Cons<T>(this.first, 
          this.rest.replaceFirstOccurrence(target, replacement, equalityPred));
    }
  }

  // folds the list from the right.
  public <R> R foldr(IFunc2<T, R, R> func, R base) {
    return func.apply(first, this.rest.foldr(func, base));
  }

  // does this Cons contain the target?
  public boolean contains(T target, IFunc2<T, T, Boolean> equalityPred) {
    return equalityPred.apply(this.first, target) || this.rest.contains(target, equalityPred);
  }

  /* Template:
   * Fields:
   * this.first                                                 -- T
   * this.rest                                                  -- IList<T>
   * 
   * Methods:
   * this.pushBack(T)                                           -- IList<T>
   * this.removeLast()                                          -- IList<T>
   * this.removeLastHelper(T)                                   -- IList<T>
   * this.reverse()                                             -- IList<T>
   * this.reverseHelper(IList<T>)                               -- IList<T>
   * this.get(int)                                              -- T
   * this.size()                                                -- int
   * this.sameList(IList<T>, IFunc2<T, T, Boolean>)             -- Boolean
   * this.sameCons(Cons<T>, IFunc2<T, T, Boolean>)              -- Boolean
   * this.sameEmpty(Empty<T>, IFunc2<T, T, Boolean>)            -- Boolean
   * this.samePositionAndValue(IList<T>, IFunc<T, T, Boolean>)  -- IList<Boolean>
   * this.containsWithReplacement(IList<T>, T, IFunc2<T, T, Boolean>)    -- IList<Boolean>
   * this.replaceFirstOccurence(T, T, IFunc2<T, T, Boolean>)    -- IList<T>
   * this.foldr(IFunc2<T, R, R>, R)                             -- R
   * this.map(IFunc<T, R>)                                      -- R
   * this.map2(IList<T2>, IFunc2<T, T2, R>)                     -- IList<R>
   * this.map2Cons(Cons<T2>, IFunc2<T, T2, R>)                  -- IList<R>
   * this.map2Mt(Empty<T2>, IFunc2<T, T2, R>)                   -- IList<R>
   * this.contains(T, IFunc2<T, T, Boolean>)                    -- Boolean
   */
}



class WordleExamples {
  Random testingRandom = new Random();
  WordList words = new WordList();

  IList<String> dictionary5 = words.split(words.fiveLetters, 5, 1);
  IList<String> dictionary4 = words.split(words.fourLetterWords, 4, 1);

  Wordle wordle5 = new Wordle(dictionary5, 5, testingRandom);
  Wordle wordle4 = new Wordle(dictionary4, 5, testingRandom);
  Dordle dordle5 = new Dordle(dictionary5, 8, testingRandom, testingRandom);

  StringEquality stringEquals = new StringEquality();

  Empty<String> empty = new Empty<String>();
  Cons<String> secret5 = new Cons<>("H", new Cons<>("E", new Cons<>("A",
      new Cons<>("R", new Cons<>("T", new Empty<String>())))));
  Cons<String> secret5Rev = new Cons<>("T", new Cons<>("R", new Cons<>("A",
      new Cons<>("E", new Cons<>("H", new Empty<String>())))));
  Cons<String> secret5_ = new Cons<>("T", new Cons<>("O", new Cons<>("O",
      new Cons<>("L", new Cons<>("S", new Empty<String>())))));
  Cons<String> secret4 = new Cons<>("H", new Cons<>("E", new Cons<>("A",
      new Cons<>("R", new Empty<String>()))));

  Cons<Pair<String, String>> zipSec5AndSec5_ = new Cons<>(new Pair<>("H", "T"), 
      new Cons<>(new Pair<>("E", "O"), new Cons<>(new Pair<>("A", "O"), 
          new Cons<>(new Pair<>("R", "L"), new Cons<>(new Pair<>("T", "S"), new Empty<>())))));

  Cons<String> guessPerfect = new Cons<String>("H", new Cons<>("E", new Cons<>("A",
      new Cons<>("R", new Cons<>("T", new Empty<String>()))))); 
  Cons<String> lettersExistsButWrongPlace = new Cons<>("R", new Cons<>("H", new Cons<>("T",
      new Cons<>("E", new Cons<>("A", new Empty<String>()))))); 
  Cons<String> twoLettersInWrongPlace = new Cons<>("E", new Cons<>("Z", new Cons<>("R",
      new Cons<>("H", new Cons<>("H", new Empty<String>()))))); 
  Cons<String> oneRightOneExtraneous = new Cons<>("H", new Cons<>("Z", new Cons<>("R",
      new Cons<>("E", new Cons<>("H", new Empty<String>())))));
  Cons<String> allWrong = new Cons<>("X", new Cons<>("Z", new Cons<>("V",
      new Cons<>("K", new Cons<>("F", new Empty<String>())))));

  Cons<Boolean> rightsBList = new Cons<>(true, 
      new Cons<>(true, new Cons<>(false, new Cons<>(true, 
          new Cons<>(false, new Empty<>())))));
  Cons<Boolean> containsBList = new Cons<>(false, new Cons<>(false, new Cons<>(true, 
      new Cons<>(false, new Cons<>(false, new Empty<>())))));
  Cons<Color> colorList = new Cons<>(Color.GREEN, new Cons<>(Color.GREEN, 
      new Cons<>(new Color(230, 230, 0), new Cons<>(Color.GREEN, 
          new Cons<>(Color.GRAY, new Empty<>())))));

  /* ********************** TESTER CHECKLIST **********************
   * 
   * Wordle:
   * this.makeScene()         -- WorldScene
   * this.onKeyEvent(String)  -- World
   * this.shouldWorldEnd()    -- boolean
   * this.lastScene(String)   -- WorldScene
   * 
   * Dordle:
   * this.makeScene()         -- WorldScene
   * this.onKeyEvent(String)  -- World
   * this.shouldWorldEnd() -- boolean
   * this.lastScene(String)   -- WorldScene
   * 
   * WordList:
   * this.split(String, int, int)   -- IList<String>
   * 
   * Util:
   * this.drawSquare()    -- WorldImage
   * this.drawSquare(String, Color)   -- WorldImage
   * this.addBlock(WorldImage, WorldImage)    -- WorldImage
   * this.addLettersToEnd(IList<String>,String, int)    -- IList<String>
   * this.drawRowOfDisabledSquares()    -- WorldImage
   * this.completeRowCurrentGuess(IList<String>)    -- WorldImage
   * this.drawRow(IList<String>, IList<String>)   -- WorldImage
   * 
   * StringAppend:
   * apply(String, String)    -- String
   * 
   * DrawRow:
   * this.apply(IList<String>)    -- WorldImage
   * 
   * GetColor:
   * this.apply(Boolean, Boolean)   -- Color
   * 
   * DrawBelow:
   * this.apply(WorldImage, WorldImage)   -- WorldImage
   * 
   * DrawLeft:
   * this.apply(Pair<Color, String>, WorldImage)    -- WorldImage
   * 
   * Zipper:
   * this.apply(T1, T2)   -- Pair<T1, T2>
   * 
   * StringEquality:
   * this.apply(String, String)   -- Boolean
   * 
   * IList<T>
   * this.pushBack(T)                                           -- IList<T>
   * this.removeLast()                                          -- IList<T>
   * this.removeLastHelper(T)                                   -- IList<T>
   * this.reverse()                                             -- IList<T>
   * this.reverseHelper(IList<T>)                               -- IList<T>
   * this.get(int)                                              -- T
   * this.size()                                                -- int
   * this.sameList(IList<T>, IFunc2<T, T, Boolean>)             -- Boolean
   * this.sameCons(Cons<T>, IFunc2<T, T, Boolean>)              -- Boolean
   * this.sameEmpty(Empty<T>, IFunc2<T, T, Boolean>)            -- Boolean
   * this.samePositionAndValue(IList<T>, IFunc<T, T, Boolean>)  -- IList<Boolean>
   * this.containsWithReplacement(IList<T>, T, IFunc2<T, T, Boolean>)    -- IList<Boolean>
   * this.replaceFirstOccurence(T, T, IFunc2<T, T, Boolean>)    -- IList<T>
   * this.foldr(IFunc2<T, R, R>, R)                             -- R
   * this.map(IFunc<T, R>)                                      -- R
   * this.map2(IList<T2>, IFunc2<T, T2, R>)                     -- IList<R>
   * this.map2Cons(Cons<T2>, IFunc2<T, T2, R>)                  -- IList<R>
   * this.map2Mt(Empty<T2>, IFunc2<T, T2, R>)                   -- IList<R>
   * this.contains(T, IFunc2<T, T, Boolean>)                    -- Boolean
   * 
   * *******************************************************************
   */

  boolean testMakeScene(Tester t) {
    return t.checkExpect(wordle5.makeScene(), new WorldScene(300, 300)
        .placeImageXY(this.wordle5.draw(),100,150))
        && t.checkExpect(wordle4.makeScene(), new WorldScene(300, 300)
            .placeImageXY(this.wordle4.draw(),100,150))
        && t.checkExpect(dordle5.makeScene(), new WorldScene(400, 400)
            .placeImageXY(dordle5.wordle1.draw(),100,200)
            .placeImageXY(dordle5.wordle2.draw(), 300, 200));
  }

  boolean testOnKeyEvent(Tester t) {
    WordList wl = new WordList();

    return t.checkExpect(wordle5.onKeyEvent("a"), // type "a"
        new Wordle(wordle5.dictionary, wordle5.numGuesses, wordle5.guesses,
            wordle5.secretWord, new Cons<String>("A", new Empty<String>()), 
            wordle5.lastInputedGuess))
        && t.checkExpect(wordle5.onKeyEvent("1"), // type "1"
            wordle5)
        && t.checkExpect(wordle5.onKeyEvent("a").onKeyEvent("backspace"), // "a" + "backspace"
            new Wordle(wordle5.dictionary, wordle5.numGuesses, wordle5.guesses,
                wordle5.secretWord, new Empty<String>(), wordle5.lastInputedGuess))
        && t.checkExpect(wordle5.onKeyEvent("backspace"), // "backspace"
            new Wordle(wordle5.dictionary, wordle5.numGuesses, wordle5.guesses,
                wordle5.secretWord, new Empty<String>(), wordle5.lastInputedGuess))
        && t.checkExpect(new Wordle(wordle5.dictionary, wordle5.numGuesses, wordle5.guesses,
            wordle5.secretWord, wl.split("UGS", 1, 0), 
            wordle5.lastInputedGuess).onKeyEvent("enter"), // "enter"
            new Wordle(wordle5.dictionary, wordle5.numGuesses, 
                wordle5.guesses,
                wordle5.secretWord, wl.split("UGS", 1, 0), wordle5.lastInputedGuess))
        && t.checkExpect(new Wordle(wordle5.dictionary, wordle5.numGuesses, wordle5.guesses,
            wordle5.secretWord, wl.split("MONKE", 1, 0), 
            wordle5.lastInputedGuess).onKeyEvent("enter"), // "enter"
            new Wordle(wordle5.dictionary, wordle5.numGuesses, 
                wordle5.guesses,
                wordle5.secretWord, wl.split("MONKE", 1, 0), wordle5.lastInputedGuess))
        && t.checkExpect(dordle5.onKeyEvent("a"), // "a"
            new Dordle((Wordle) dordle5.wordle1.onKeyEvent("a"), 
                (Wordle)dordle5.wordle2.onKeyEvent("a")));
  }

  boolean testShouldWorldEnd(Tester t) {
    Wordle endedWordle0 = new Wordle(dictionary5, 5, testingRandom);
    endedWordle0.numGuesses = 0;

    Wordle endedWordle1 = new Wordle(dictionary5, 5, testingRandom);
    endedWordle1.lastInputedGuess = guessPerfect;
    endedWordle1.secretWord = secret5;

    Wordle notEnded = new Wordle(dictionary5, 5, testingRandom);

    return t.checkExpect(endedWordle0.shouldWorldEnd(), true)
        && t.checkExpect(endedWordle1.shouldWorldEnd(), true)
        && t.checkExpect(notEnded.shouldWorldEnd(), false);
  }

  boolean testLastScene(Tester t) {
    return t.checkExpect(wordle5.lastScene(""), wordle5.makeScene())
        && t.checkExpect(wordle4.lastScene(""), wordle4.makeScene())
        && t.checkExpect(dordle5.lastScene(""), dordle5.makeScene());
  }

  boolean testDraw(Tester t) {
    Util ut = new Util();
    Util ut4 = new Util(4);

    return t.checkExpect(wordle5.draw(), 
        (WorldImage)new AboveImage(
            (wordle5.guesses.map(new DrawRow(wordle5.secretWord)))
            .foldr(new DrawBelow(), new EmptyImage()), 
            ut.completeRowCurrentGuess(wordle5.currentGuess), 
            (new Cons<WorldImage>(ut.drawRowOfDisabledSquares(), 4))
            .foldr(new DrawBelow(), new EmptyImage())))
        && t.checkExpect(wordle4.draw(), 
            (WorldImage)new AboveImage(
                (wordle4.guesses.map(new DrawRow(wordle4.secretWord)))
                .foldr(new DrawBelow(), new EmptyImage()), 
                ut4.completeRowCurrentGuess(wordle4.currentGuess), 
                (new Cons<WorldImage>(ut4.drawRowOfDisabledSquares(), 4))
                .foldr(new DrawBelow(), new EmptyImage())));
  }

  boolean testSplit(Tester t) {
    return t.checkExpect(words.split("HEART", 1, 0), secret5)
        && t.checkExpect(words.split("Extra,", 5, 1),
            new Cons<>("Extra", new Empty<String>()))
        && t.checkExpect(words.split("Fit", 3, 1),
            new Cons<>("Fit", new Empty<String>()));
  }

  boolean testDrawSquare(Tester t) {
    Util ut = new Util();

    return t.checkExpect(ut.drawSquare(), ut.drawSquare("", Color.GRAY))
        && t.checkExpect(ut.drawSquare("A",Color.RED), 
            new OverlayImage(new TextImage("A", 30, Color.WHITE), 
                new OverlayImage(new RectangleImage(30, 30, OutlineMode.OUTLINE, Color.GRAY),
                    new RectangleImage(30, 30, OutlineMode.SOLID, Color.RED))))
        && t.checkExpect(ut.drawSquare("A",Color.GREEN), 
            new OverlayImage(new TextImage("A", 30, Color.WHITE), 
                new OverlayImage(new RectangleImage(30, 30, OutlineMode.OUTLINE, Color.GRAY),
                    new RectangleImage(30, 30, OutlineMode.SOLID, Color.GREEN))));
  }

  boolean testAddBlock(Tester t) {
    Util ut = new Util();

    return t.checkExpect(ut.addBlock(ut.drawSquare(), 
        ut.drawSquare("A", Color.RED)),
        new BesideImage(ut.drawSquare(), ut.buffer, ut.drawSquare("A", Color.RED)))
        && t.checkExpect(ut.addBlock(ut.drawSquare("B", Color.GREEN), 
            ut.drawSquare("A", Color.RED)),
            new BesideImage(ut.drawSquare("B", Color.GREEN), ut.buffer, 
                ut.drawSquare("A", Color.RED)));
  }

  boolean testAddLettersToEnd(Tester t) {
    Util ut = new Util();

    return t.checkExpect(ut.addLettersToEnd(new Cons<String>("A", 5), "B", 2),
        new Cons<String>("A", 5).pushBack("B").pushBack("B"))
        && t.checkExpect(ut.addLettersToEnd(new Cons<String>("A", 5), "B", 0),
            new Cons<String>("A", 5));
  }

  boolean testDrawRowOfDisabledSquares(Tester t) {
    Util ut = new Util();

    return t.checkExpect(ut.drawRowOfDisabledSquares(), 
        (new Cons<Color>(Color.GRAY, 5))
        .map2(new Cons<String>("", 5), new Zipper<>())
        .foldr(new DrawLeft(), new EmptyImage()));
  }

  boolean testCompleteRowCurrentGuess(Tester t) {
    Util ut = new Util();

    return t.checkExpect(ut.completeRowCurrentGuess(new Empty<String>()),
        ut.drawRowOfDisabledSquares())
        && t.checkExpect(ut.completeRowCurrentGuess(new Cons<String>("A", 1)),
            ut.drawRow(ut.addLettersToEnd(new Cons<String>("A", 1),"",4), 
                new Cons<String>("_", 5)))
        && t.checkExpect(ut.completeRowCurrentGuess(new Cons<String>("A", 1)),
            ut.drawRow(ut.addLettersToEnd(new Cons<String>("A", 1), "", 4), 
                new Cons<String>("_", 5)))
        && t.checkExpect(ut.completeRowCurrentGuess(new Cons<String>("A", 2)),
            ut.drawRow(ut.addLettersToEnd(new Cons<String>("A", 2), "", 3), 
                new Cons<String>("_", 5)));
  }

  boolean testdrawRow(Tester t) {
    Util ut = new Util();

    return t.checkExpect(ut.drawRow(ut.addLettersToEnd(new Cons<String>("A", 1),"B",4), 
        new Cons<String>("A", 3).pushBack("B").pushBack("C")), 
        new BesideImage(
            ut.drawSquare("A", Color.GREEN), ut.buffer,
            new BesideImage(ut.drawSquare("B", Color.GRAY), ut.buffer,
                new BesideImage(ut.drawSquare("B", Color.GRAY), ut.buffer,
                    new BesideImage(ut.drawSquare("B", Color.GREEN), ut.buffer,
                        new BesideImage(ut.drawSquare("B", Color.GRAY), ut.buffer,
                            new EmptyImage()))))))
        && t.checkExpect(ut.drawRow(ut.addLettersToEnd(new Cons<String>("A", 1),"B",4), 
            new Cons<String>("A", 3).pushBack("B").pushBack("C")), 
            new BesideImage(
                ut.drawSquare("A", Color.GREEN), ut.buffer,
                new BesideImage(ut.drawSquare("B", Color.GRAY), ut.buffer,
                    new BesideImage(ut.drawSquare("B", Color.GRAY), ut.buffer,
                        new BesideImage(ut.drawSquare("B", Color.GREEN), ut.buffer,
                            new BesideImage(ut.drawSquare("B", Color.GRAY), ut.buffer,
                                new EmptyImage()))))));
  }

  boolean testStringAppend(Tester t) {
    StringAppend sa = new StringAppend();

    return t.checkExpect(sa.apply("what", " up"), "what up")
        && t.checkExpect(sa.apply("check", " 1 2"), "check 1 2")
        && t.checkExpect(sa.apply("empty?", ""), "empty?");
  }

  boolean testDrawRow(Tester t) {
    WordList wl = new WordList();
    DrawRow dr = new DrawRow(wl.split("HELLO", 1, 0));
    Util ut = new Util();

    return t.checkExpect(dr.apply(wl.split("HEAPS", 1, 0)),
        ut.drawRow(wl.split("HEAPS", 1, 0), wl.split("HELLO", 1, 0)))
        && t.checkExpect(dr.apply(wl.split("HEIST", 1, 0)),
            ut.drawRow(wl.split("HEIST", 1, 0), wl.split("HELLO", 1, 0)))
        && t.checkExpect(dr.apply(wl.split("WALKS", 1, 0)),
            ut.drawRow(wl.split("WALKS", 1, 0), wl.split("HELLO", 1, 0)));
  }

  boolean testGetColor(Tester t) {
    GetCorrectColor gcc = new GetCorrectColor();

    return t.checkExpect(gcc.apply(true, true), Color.GREEN)
        && t.checkExpect(gcc.apply(false, true), new Color(230, 230, 0))
        && t.checkExpect(gcc.apply(true, true), Color.GREEN)
        && t.checkExpect(gcc.apply(false, false), Color.GRAY);
  }

  boolean testDrawBelow(Tester t) {
    DrawBelow db = new DrawBelow();
    Util ut = new Util();

    return t.checkExpect(db.apply(ut.drawSquare(), ut.drawSquare()),
        new AboveImage(ut.drawSquare(), ut.drawSquare()))
        && t.checkExpect(db.apply(ut.drawSquare("A", Color.GREEN), ut.drawSquare()),
            new AboveImage(ut.drawSquare(), ut.drawSquare("A", Color.GREEN)))
        && t.checkExpect(db.apply(ut.drawSquare("B", Color.RED), ut.drawSquare("A", Color.GREEN)),
            new AboveImage(ut.drawSquare("A", Color.GREEN), ut.drawSquare("B", Color.RED)));
  }

  boolean testDrawLeft(Tester t) {
    DrawLeft dl = new DrawLeft();
    Util ut = new Util();

    return t.checkExpect(dl.apply(new Pair<Color, String>(Color.RED, "A"), ut.drawSquare()),
        ut.addBlock(ut.drawSquare("A", Color.RED), ut.drawSquare()))
        && t.checkExpect(dl.apply(new Pair<Color, String>(Color.GREEN, "A"), ut.drawSquare()),
            ut.addBlock(ut.drawSquare("A", Color.GREEN), ut.drawSquare()))
        && t.checkExpect(dl.apply(new Pair<Color, String>(Color.RED, "B"), 
            ut.drawSquare("A", Color.RED)),
            ut.addBlock(ut.drawSquare("B", Color.RED), ut.drawSquare("A", Color.RED)));
  }

  boolean testZipper(Tester t) {
    Zipper<String, String> z = new Zipper<String, String>();

    return t.checkExpect(z.apply("s1", "s2"), new Pair<String, String>("s1", "s2"))
        && t.checkExpect(z.apply("s2", "s1"), new Pair<String, String>("s2", "s1"));
  }

  boolean testStringEquality(Tester t) {
    StringEquality se = new StringEquality();

    return t.checkExpect(se.apply("s1", "s2"), false)
        && t.checkExpect(se.apply("s1", "s1"), true);
  }

  boolean testReplaceWhenTrue(Tester t) {
    ReplaceWhenTrue rwt = new ReplaceWhenTrue();

    return t.checkExpect(rwt.apply("WHAT", true), "_")
        && t.checkExpect(rwt.apply("OK", false), "OK");
  }

  boolean testPushBack(Tester t) {
    return t.checkExpect(empty.pushBack("A"), new Cons<String>("A", 1))
        && t.checkExpect(secret5.pushBack("B"), words.split("HEARTB", 1, 0));
  }

  boolean testRemoveLast(Tester t) {
    return t.checkExpect(empty.removeLast(), empty)
        && t.checkExpect(empty.pushBack("A").removeLast(), empty)
        && t.checkExpect(secret5.removeLast(), words.split("HEAR", 1, 0));
  }

  boolean testReverse(Tester t) {
    return t.checkExpect(empty.reverse(), empty)
        && t.checkExpect(secret5.reverse(), secret5Rev)
        && t.checkExpect(secret5.reverse(), words.split("TRAEH", 1, 0))
        && t.checkExpect(empty.reverseHelper(empty), empty)
        && t.checkExpect(empty.reverseHelper(secret5), secret5);
  }

  boolean testGet(Tester t) {
    return t.checkException(new RuntimeException("List index out of bounds"), empty, "get", 0)
        && t.checkException(new RuntimeException("List index out of bounds"), secret5, "get", 5)
        && t.checkExpect(secret5.get(4), "T");
  }

  boolean testSize(Tester t) {
    return t.checkExpect(empty.size(), 0)
        && t.checkExpect(secret5.size(), 5)
        && t.checkExpect(empty.size(), 0)
        && t.checkExpect(secret5.size(), 5);
  }

  boolean testSameList(Tester t) {
    return t.checkExpect(secret5.sameList(guessPerfect, stringEquals), true)
        && t.checkExpect(secret5.sameList(lettersExistsButWrongPlace, stringEquals), false)
        && t.checkExpect(secret5.sameList(allWrong, stringEquals), false)
        && t.checkExpect(empty.sameList(empty, stringEquals), true)
        && t.checkExpect(empty.sameList(allWrong, stringEquals), false);
  }

  boolean testSameCons(Tester t) {
    return t.checkExpect(secret5.sameCons(guessPerfect, stringEquals), true)
        && t.checkExpect(secret5.sameCons(lettersExistsButWrongPlace, stringEquals), false)
        && t.checkExpect(empty.sameCons(allWrong, stringEquals), false);
  }

  boolean testSameEmpty(Tester t) {
    return t.checkExpect(secret5.sameEmpty(empty, stringEquals), false)
        && t.checkExpect(empty.sameEmpty(empty, stringEquals), true);
  }

  boolean testSamePositionAndValue(Tester t) {
    Boolean trueB = true;
    Boolean falseB = false;
    IList<Boolean> allFalse = new Cons<>(falseB, new Cons<>(falseB, new Cons<>(falseB,
        new Cons<>(falseB, new Cons<>(falseB, new Empty<Boolean>())))));

    return t.checkExpect(secret5.samePositionAndValue(guessPerfect, stringEquals), 
        new Cons<>(trueB, new Cons<>(trueB, new Cons<>(trueB,
            new Cons<>(trueB, new Cons<>(trueB, new Empty<Boolean>()))))))
        && t.checkExpect(secret5.samePositionAndValue(lettersExistsButWrongPlace, stringEquals),
            allFalse)
        && t.checkExpect(secret5.samePositionAndValue(twoLettersInWrongPlace, stringEquals), 
            allFalse)
        && t.checkExpect(secret5.samePositionAndValue(allWrong, stringEquals), allFalse);
  }

  boolean testContainsWithReplacement(Tester t) {
    Boolean trueB = true;
    Boolean falseB = false;
    IList<Boolean> allFalse = new Cons<>(falseB, new Cons<>(falseB, new Cons<>(falseB,
        new Cons<>(falseB, new Cons<>(falseB, new Empty<Boolean>())))));
    IList<Boolean> allTrue = new Cons<>(trueB, new Cons<>(trueB, new Cons<>(trueB,
        new Cons<>(trueB, new Cons<>(trueB, new Empty<Boolean>())))));

    return t.checkExpect(secret5.containsWithReplacement(guessPerfect, "_", stringEquals), 
        allTrue)
        && t.checkExpect(secret5.containsWithReplacement(lettersExistsButWrongPlace, "_", 
            stringEquals),
            allTrue)
        && t.checkExpect(secret5.containsWithReplacement(twoLettersInWrongPlace, "_", 
            stringEquals), 
            new Cons<>(trueB, new Cons<>(trueB, new Cons<>(falseB,
                new Cons<>(trueB, 
                    new Cons<>(falseB, new Empty<Boolean>())))))) // second "H" won't be yellow
        // because there is only one H in the secret word
        && t.checkExpect(secret5.containsWithReplacement(allWrong, "_", stringEquals), allFalse);
  }

  boolean testReplaceFirstOccurance(Tester t) {
    return t.checkExpect(secret5.replaceFirstOccurrence("H", "_", stringEquals), 
        new Cons<>("_", new Cons<>("E", new Cons<>("A", new Cons<>("R",
            new Cons<>("T", new Empty<String>()))))))
        && t.checkExpect(twoLettersInWrongPlace.replaceFirstOccurrence("H", "_", stringEquals), 
            new Cons<>("E", new Cons<>("Z", new Cons<>("R", new Cons<>("_",
                new Cons<>("H", new Empty<String>()))))));
  }

  boolean testFoldr(Tester t) {
    IList<String> s = new Cons<String>("A", 5);

    return t.checkExpect(s.foldr(new StringAppend(), ""), "AAAAA")
        && t.checkExpect(words.split("APPLE", 1, 0).foldr(new StringAppend(), ""), "APPLE")
        && t.checkExpect(words.split("WORDS", 1, 0).foldr(new StringAppend(), ""), "WORDS")
        && t.checkExpect(words.split("MONKEY", 1, 0).foldr(new StringAppend(), ""), "MONKEY");
  }

  // tests both Zipper and GetCorrectColor
  boolean testMap2(Tester t) {
    return t.checkExpect(secret5.map2(secret5_, new Zipper<>()), zipSec5AndSec5_)
        && t.checkException(new RuntimeException("List arguments are different lengths"), 
            secret5, "map2", secret4, new Zipper<>())
        && t.checkExpect(rightsBList.map2(containsBList, new GetCorrectColor()), colorList);
  }

  boolean testContains(Tester t) {
    return t.checkExpect(secret5.contains("H", new StringEquality()), true)
        && t.checkExpect(secret5.contains("X", new StringEquality()), false)
        && t.checkExpect(empty.contains("Z", new StringEquality()), false)
        && t.checkExpect(empty.contains("Something", stringEquals), false)
        && t.checkExpect(secret5.contains("A", stringEquals), true) 
        && t.checkExpect(secret5.contains("X", stringEquals), false);
  }



  boolean testWorldGen(Tester t) {
    wordle5.bigBang(300, 300, 1);
    // wordle4.bigBang(300, 300, 1);
    // dordle5.bigBang(400, 400, 1);
    return true;
  }
}