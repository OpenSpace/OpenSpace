import React, { Component } from 'react';
import { Link } from 'react-router-dom';
import DataManager from '../../api/DataManager';
import Icon from '../common/Icon/Icon';
import Button from '../common/Input/Button/Button';
import styles from './SystemMenu.scss';
import Popover from '../common/Popover/Popover';
import { ShutdownScript, ToggleConsoleScript } from '../../api/keys';

class SystemMenu extends Component {
  static quit() {
    DataManager.runScript(ShutdownScript);
  }

  static console() {
    DataManager.runScript(ToggleConsoleScript);
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
            <nav className={styles.links}>
              <button onClick={SystemMenu.console}>
                Toggle console <span className={styles.shortcut}>~</span>
              </button>

              <hr className={Popover.styles.delimiter} />

              <button onClick={SystemMenu.quit}>
                Quit OpenSpace <span className={styles.shortcut}>ESC</span>
              </button>

              <hr className={Popover.styles.delimiter} />

              <a
                href="https://github.com/OpenSpace/OpenSpace/wiki/General-Getting-Started-Guide-Using-OpenSpace"
                target="_blank"
                rel="noopener noreferrer"
              >
                <Icon icon="help" /> Help
              </a>
              <a
                href="https://github.com/OpenSpace/OpenSpace/issues/new"
                target="_blank"
                rel="noopener noreferrer"
              >
                <Icon icon="report_problem" /> Report a problem
              </a>
              <a
                href="http://openspaceproject.com/"
                target="_blank"
                rel="noopener noreferrer"
              >
                <Icon icon="public" /> openspaceproject.com
              </a>
              <Link to="about">
                <Icon icon="info" /> About
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
