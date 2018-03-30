import React, { Component } from 'react';
import { Link } from 'react-router-dom';
import DataManager from '../../api/DataManager';
import Icon from '../common/Icon/Icon';
import Button from '../common/Input/Button/Button';
import styles from './SystemMenu.scss';
import Popover from '../common/Popover/Popover';
import {
  ShutdownScript,
  ToggleConsoleScript,
  ToggleNativeGuiScript } from '../../api/keys';

class SystemMenu extends Component {
  static quit() {
    DataManager.runScript(ShutdownScript);
  }

  static console() {
    DataManager.runScript(ToggleConsoleScript);
  }

  static nativeGui() {
    DataManager.runScript(ToggleNativeGuiScript);
  }

  constructor(props) {
    super(props);
    this.state = { showMenu: false };
    this.toggleMenu = this.toggleMenu.bind(this);
  }

  toggleMenu() {
    this.setState({ showMenu: !this.state.showMenu });
  }

  render() {
    return (
      <div className={styles.SystemMenu}>
        { this.state.showMenu && (
          <Popover className={styles.popover} arrow="arrow bottom leftside">
            <nav className={styles.links} onClick={this.toggleMenu}>
              <button onClick={SystemMenu.console}>
                Toggle console <span className={styles.shortcut}>~</span>
              </button>
              <button onClick={SystemMenu.nativeGui}>
                Toggle native GUI <span className={styles.shortcut}>F3</span>
              </button>

              <hr className={Popover.styles.delimiter} />

              <button onClick={SystemMenu.quit}>
                <Icon icon="exit_to_app" className={styles.linkIcon} />
                Quit OpenSpace <span className={styles.shortcut}>ESC</span>
              </button>

              <hr className={Popover.styles.delimiter} />

              <a
                href="https://github.com/OpenSpace/OpenSpace/wiki/General-Getting-Started-Guide-Using-OpenSpace"
                target="_blank"
                rel="noopener noreferrer"
              >
                <Icon icon="help" className={styles.linkIcon} />
                Help
              </a>
              <a
                href="https://github.com/OpenSpace/OpenSpace/issues/new"
                target="_blank"
                rel="noopener noreferrer"
              >
                <Icon icon="bug_report" className={styles.linkIcon} />
                Report a problem
              </a>
              <a
                href="http://openspaceproject.com/"
                target="_blank"
                rel="noopener noreferrer"
              >
                <Icon icon="public" className={styles.linkIcon} />
                openspaceproject.com
              </a>
              <Link to="about">
                <Icon icon="info" className={styles.linkIcon} />
                About
              </Link>
            </nav>
          </Popover>
        )}

        <Button className={styles.button} transparent onClick={this.toggleMenu}>
          <Icon icon="more_vert" className={styles.icon} />
        </Button>
      </div>
    );
  }
}

export default SystemMenu;
