package controllers

import models.entities.Repository

import scala.language.higherKinds

/*
 * Copyright 2014(23.11.14) Arakcheev Artem (artem.arakcheev@phystech.edu)
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

object RepositoryController extends JsonSerializerController with Secured {

  /**
   * List of user repositories
   * @return
   */
  def list = Auth.async(parse.anyContent) { implicit user => implicit request =>
    >>!(Repository list)
  }

  /**
   * Create new repository with name ${name}
   * @param name
   * @return
   */
  def newRepo(name: String) = Auth.async(parse.anyContent) { implicit user => implicit request =>
    !>>(Repository gen name)
  }

  def delete(id: String) = Auth.async(parse.anyContent) { implicit user => implicit request =>
    !>>(Repository del id)
  }
}
