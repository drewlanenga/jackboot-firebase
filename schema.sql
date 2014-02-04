--
-- Database: `ncaa_bb`
--

-- --------------------------------------------------------

--
-- Table structure for table `boxscores`
--

CREATE TABLE IF NOT EXISTS `boxscores` (
  `gameid` int(11) NOT NULL,
  `teamid` int(11) NOT NULL,
  `playerid` int(11) NOT NULL,
  `min` int(11) NOT NULL,
  `fgm` int(11) NOT NULL,
  `fga` int(11) NOT NULL,
  `threem` int(11) NOT NULL,
  `threea` int(11) NOT NULL,
  `ftm` int(11) NOT NULL,
  `fta` int(11) NOT NULL,
  `oreb` int(11) NOT NULL,
  `dreb` int(11) NOT NULL,
  `ast` int(11) NOT NULL,
  `stl` int(11) NOT NULL,
  `blk` int(11) NOT NULL,
  `to` int(11) NOT NULL,
  `pf` int(11) NOT NULL,
  `pts` int(11) NOT NULL,
  PRIMARY KEY (`gameid`,`teamid`,`playerid`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

-- --------------------------------------------------------

--
-- Table structure for table `schedule`
--

CREATE TABLE IF NOT EXISTS `schedule` (
  `gameid` int(11) NOT NULL,
  `homeid` int(11) NOT NULL,
  `visitid` int(11) NOT NULL,
  `homescore` int(11) NOT NULL,
  `visitscore` int(11) NOT NULL,
  `year` int(11) NOT NULL,
  `seasontype` int(11) NOT NULL,
  `gamedate` date NOT NULL,
  PRIMARY KEY (`gameid`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

-- --------------------------------------------------------

--
-- Table structure for table `teams`
--

CREATE TABLE IF NOT EXISTS `teams` (
  `teamid` int(11) NOT NULL,
  `kaggleid` int(11) NOT NULL,
  `teamname` varchar(255) NOT NULL,
  PRIMARY KEY (`teamid`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
