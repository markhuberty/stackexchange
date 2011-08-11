SET SQL_MODE="NO_AUTO_VALUE_ON_ZERO";

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;


CREATE TABLE IF NOT EXISTS `badges` (
  `ID` mediumint(8) unsigned NOT NULL,
  `USERID` mediumint(9) NOT NULL,
  `NAME` enum('.htaccess','.net','.net-3.5','algorithm','Altruist','android','apache','arrays','asp.net','asp.net-mvc','Autobiographer','bash','Benefactor','Beta','big-o','blackberry','c','c#','c#3.0','c++','career-development','casting','Citizen Patrol','Civic Duty','Cleanup','clojure','clr','cocoa','cocoa-touch','coding-style','coldfusion','collections','Commentator','common-lisp','compact-framework','complexity','computer-science','constructor','Copy Editor','core-data','Critic','css','database','datetime','delegates','delphi','dependency-injection','dictionary','Disciplined','django','drupal','eclipse','Editor','Electorate','emacs','Enlightened','Enthusiast','entity-framework','enums','Epic','events','exception','extension-methods','f#','Famous Question','Fanatic','Favorite Question','functional-programming','Generalist','generics','git','Good Answer','Good Question','google-app-engine','Great Answer','Great Question','Guru','haskell','hibernate','html','ienumerable','inheritance','interview-questions','Investor','iphone','java','javascript','jpa','jquery','jsf','jsp','lambda','language-agnostic','Legendary','linq','linq-to-sql','linux','lisp','list','matlab','maven-2','mercurial','mod-rewrite','mono','Mortarboard','ms-access','multithreading','mysql','Necromancer','Nice Answer','Nice Question','Notable Question','objective-c','oop','optimization','oracle','Organizer','orm','Peer Pressure','performance','perl','php','pointers','Popular Question','Populist','powershell','programming-languages','Promoter','Pundit','python','query','r','reflection','regex','rest','Reversal','ruby','ruby-on-rails','scala','Scholar','security','Self-Learner','serialization','servlets','sharepoint','silverlight','spring','sql','sql-server','sql-server-2005','static','Stellar Question','stl','string','Strunk &amp; White','Student','subsonic','Supporter','svn','swing','Taxonomist','Teacher','templates','Tenacious','tsql','Tumbleweed','unit-testing','Unsung Hero','vb.net','vb6','version-control','visual-studio','visual-studio-2008','wcf','windows','windows-mobile','winforms','wpf','xml','xpath','xslt','Yearling') COLLATE utf8_unicode_ci DEFAULT NULL,
  `DATE` datetime NOT NULL,
  PRIMARY KEY (`ID`),
  KEY `USERID` (`USERID`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;

CREATE TABLE IF NOT EXISTS `comments` (
  `ID` mediumint(8) unsigned NOT NULL,
  `POSTID` mediumint(8) unsigned NOT NULL,
  `SCORE` smallint(5) unsigned NOT NULL,
  `TEXT` text COLLATE utf8_unicode_ci NOT NULL,
  `CREATIONDATE` datetime NOT NULL,
  `USERID` mediumint(8) unsigned NOT NULL,
  PRIMARY KEY (`ID`),
  KEY `POSTID` (`POSTID`,`USERID`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;

CREATE TABLE IF NOT EXISTS `posts` (
  `ID` mediumint(8) unsigned NOT NULL,
  `POSTTYPEID` enum('1','2','3') COLLATE utf8_unicode_ci NOT NULL,
  `PARENTID` mediumint(8) unsigned NOT NULL,
  `ACCEPTEDANSWERID` mediumint(8) unsigned DEFAULT NULL,
  `CREATIONDATE` datetime NOT NULL,
  `SCORE` smallint(5) unsigned NOT NULL,
  `VIEWCOUNT` mediumint(8) unsigned NOT NULL,
  `BODY` text COLLATE utf8_unicode_ci NOT NULL,
  `OWNERUSERID` mediumint(9) NOT NULL,
  `LASTEDITORUSERID` mediumint(9) NOT NULL,
  `LASTEDITORDISPLAYNAME` tinytext COLLATE utf8_unicode_ci NOT NULL,
  `LASTEDITDATE` datetime NOT NULL,
  `LASTACTIVITYDATE` datetime NOT NULL,
  `COMMUNITYOWNEDDATE` datetime NOT NULL,
  `CLOSEDDATE` datetime NOT NULL,
  `TITLE` tinytext COLLATE utf8_unicode_ci NOT NULL,
  `TAGS` varchar(115) COLLATE utf8_unicode_ci NOT NULL,
  `ANSWERCOUNT` smallint(5) unsigned NOT NULL,
  `COMMENTCOUNT` tinyint(10) unsigned NOT NULL,
  `FAVORITECOUNT` smallint(10) unsigned NOT NULL,
  PRIMARY KEY (`ID`),
  KEY `PARENTID` (`PARENTID`),
  KEY `OWNERUSERID` (`OWNERUSERID`),
  KEY `SCORE` (`SCORE`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;

CREATE TABLE IF NOT EXISTS `users` (
  `ID` mediumint(9) NOT NULL,
  `REPUTATION` mediumint(8) unsigned NOT NULL,
  `CREATIONDATE` datetime NOT NULL,
  `DISPLAYNAME` tinytext COLLATE utf8_unicode_ci NOT NULL,
  `EMAILHASH` char(32) COLLATE utf8_unicode_ci NOT NULL,
  `LASTACCESSDATE` datetime NOT NULL,
  `WEBSITEURL` varchar(255) COLLATE utf8_unicode_ci NOT NULL,
  `LOCATION` varchar(255) COLLATE utf8_unicode_ci NOT NULL,
  `AGE` tinyint(3) unsigned NOT NULL,
  `ABOUTME` text COLLATE utf8_unicode_ci NOT NULL,
  `VIEWS` mediumint(8) unsigned NOT NULL,
  `UPVOTES` mediumint(8) unsigned NOT NULL,
  `DOWNVOTES` mediumint(8) unsigned NOT NULL,
  PRIMARY KEY (`ID`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;

CREATE TABLE IF NOT EXISTS `votes` (
  `ID` mediumint(8) unsigned NOT NULL,
  `POSTID` mediumint(8) unsigned NOT NULL,
  `VOTETYPEID` enum('1','2','3','4','5','6','8','9','10','11','12','13') COLLATE utf8_unicode_ci NOT NULL,
  `CREATIONDATE` datetime NOT NULL,
  `USERID` mediumint(8) unsigned DEFAULT NULL,
  `BOUNTYAMOUNT` enum('0','25','50','75','100','125','150','175','200','225','250','275','300','350','400','450','500','550') COLLATE utf8_unicode_ci DEFAULT NULL,
  PRIMARY KEY (`ID`),
  KEY `USERID` (`USERID`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
