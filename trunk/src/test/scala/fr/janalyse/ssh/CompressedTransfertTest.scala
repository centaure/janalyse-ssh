/*
 * Copyright 2013 David Crosson
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package fr.janalyse.ssh

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import scala.io.Source
import scala.util.Properties
import java.io.File
import java.io.IOException
import scala.collection.parallel.ForkJoinTaskSupport
import org.scalatest.OptionValues._
import scalax.file._
import scalax.file.ImplicitConversions._

@RunWith(classOf[JUnitRunner])
class CompressedTransfertTest extends FunSuite with ShouldMatchers with SomeHelp {

  import util.Properties.{ userName => user }
  val opts = SSHOptions("127.0.0.1", username = user)


  test("simple") {
    val content = "Hello world"
    val testedfile = "testme-tobecompressed.txt"
    val gztestedfile = testedfile + ".gz"
    val gztestedfileMD5 = "00ec8273ecfb3c36b7e3c711f0628f8e"
      
    SSH.ftp(opts) { _.put(content, testedfile) }
    SSH.ftp(opts) { _.get(testedfile) } should equal(Some(content))
    def doclean = {
      Path(testedfile).delete()
      Path(gztestedfile).delete()
    }
    // Now let's test the compressed feature
    doclean
    SSH.once(opts) { ssh =>
      ssh.receive(testedfile, testedfile)
      Path(testedfile).string should equal(content)
      ssh.receiveNcompress(testedfile, testedfile)
      Path(gztestedfile).exists() should equal(true)
      ssh.localmd5sum(gztestedfile) should equal(Some(gztestedfileMD5))
    }
    doclean
    SSH.shellAndFtp(opts) { (_, ftp) =>
      ftp.receive(testedfile, testedfile)
      Path(testedfile).string should equal(content)
      ftp.receiveNcompress(testedfile, testedfile)
      Path(gztestedfile).exists() should equal(true)
      ftp.localmd5sum(gztestedfile) should equal(Some(gztestedfileMD5))
    }
    doclean
    SSH.ftp(opts) { ftp =>
      ftp.receive(testedfile, testedfile)
      Path(testedfile).string should equal(content)
      ftp.receiveNcompress(testedfile, testedfile)
      Path(gztestedfile).exists() should equal(true)
      ftp.localmd5sum(gztestedfile) should equal(Some(gztestedfileMD5))
    }
  }

}
